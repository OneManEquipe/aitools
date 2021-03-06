import asyncio
import logging
import typing
from contextlib import contextmanager
from typing import Iterable

from aitools.logic.core import Expression, LogicObject, Variable
from aitools.logic.language import Language
from aitools.logic.unification import Substitution
from aitools.logic.utils import normalize_variables, VariableSource
from aitools.proofs.components import HandlerArgumentMode, HandlerSafety
from aitools.proofs.listeners import Listener, PonderMode, TriggeringFormula
from aitools.proofs.provers import Proof, Prover
from aitools.storage.base import LogicObjectStorage
from aitools.storage.implementations.dummy import DummyAbstruseIndex
from aitools.storage.index import AbstruseIndex, make_key
from aitools.utils import asynctools
from aitools.utils.asynctools import process_with_loopback

logger = logging.getLogger(__name__)


class KnowledgeBase:

    def __init__(self, storage: LogicObjectStorage):
        self._storage = storage
        self._language = Language()
        self._variable_source = VariableSource(language=self._language)

        self._prover_storage: AbstruseIndex[Prover] = DummyAbstruseIndex()
        self._listener_storage: AbstruseIndex[Listener] = DummyAbstruseIndex()

        self._scheduler: asynctools.Scheduler = asynctools.Scheduler()

        self.knowledge_retriever: Prover = self.__make_knowledge_retriever()

    def supports_transactions(self) -> bool:
        return self._storage.supports_transactions()

    @contextmanager
    def transaction(self):
        with self._storage.transaction():
            yield

    def commit(self):
        self._storage.commit()

    def rollback(self):
        self._storage.rollback()

    def __make_knowledge_retriever(self):
        async def retrieve_knowledge(formula, substitution):
            async for substitution in self._retrieve(formula, previous_substitution=substitution):
                yield substitution

        # this is impure to ensure it is always called during proof verification
        return Prover(
            listened_formula=Variable(language=self._language), handler=retrieve_knowledge,
            argument_mode=HandlerArgumentMode.RAW, pass_substitution_as=..., pure=False, safety=HandlerSafety.SAFE
        )

    async def _retrieve(self, formula: Expression, *,
                  previous_substitution: Substitution = None) -> typing.AsyncIterable[Substitution]:
        """Retrieves all formula from the KnowledgeBase which are unifiable with the given one.
        No proofs are searched, so either a formula is **IN** the KB, or nothing will be returned."""
        # TODO here I am performing unification twice! I need to optimize this
        for expr, _ in self._storage.search_unifiable(other=formula):
            normalized_expr, _ = normalize_variables(expr, language=self._language)
            subst = Substitution.unify(
                normalized_expr, formula,
                previous=previous_substitution
            )

            if subst is not None:
                yield subst

    def add_formulas(self, *formulas: Expression):
        """Adds all of the given formulas to the currently known formulas."""
        normalized_formulas = (normalize_variables(f, variable_source=self._variable_source)
                               for f in formulas)
        formulas = tuple(res for res, _ in normalized_formulas)

        self._storage.add(*formulas)

    def add_prover(self, prover: Prover):
        key = make_key(prover.listened_formula)
        self._prover_storage.add(key, prover)

    def add_listener(self, listener: Listener):
        key = make_key(listener.listened_formula)
        self._listener_storage.add(key, listener)

    def __len__(self):
        return len(self._storage)

    def prove(self, formula: LogicObject, *, retrieve_only: bool = False,
              previous_substitution: typing.Optional[Substitution] = None) -> typing.Generator[Proof, None, None]:
        logger.info("Trying to prove %s with previous substitution %s", formula, previous_substitution)
        if asynctools.is_inside_task():
            raise RuntimeError(f"{KnowledgeBase.__name__}.{KnowledgeBase.prove.__name__} cannot be used "
                               f"inside tasks")

        proof_process = self._prepare_proof_sources(formula=formula, retrieve_only=retrieve_only,
                                                    previous_substitution=previous_substitution)

        for proof in self._scheduler.schedule_generator(
                proof_process, buffer_size=0
        ):
            yield proof

    async def async_prove(
            self, formula: LogicObject, *, retrieve_only: bool = False,
            previous_substitution: typing.Optional[Substitution] = None
    ) -> typing.AsyncGenerator[Proof, None]:
        logger.info("Trying to asynchronously prove %s with previous substitution %s", formula, previous_substitution)

        if not asynctools.is_inside_task():
            raise RuntimeError(f"{KnowledgeBase.__name__}.{KnowledgeBase.async_prove.__name__} cannot be used "
                               f"outside tasks")

        proof_process = self._prepare_proof_sources(formula=formula, retrieve_only=retrieve_only,
                                                    previous_substitution=previous_substitution)

        async for proof in proof_process:
            yield proof

    def _prepare_proof_sources(self, *, formula, retrieve_only, previous_substitution):
        previous_substitution = previous_substitution or Substitution()

        proof_sources: typing.List[typing.AsyncIterable[Proof]] = [
            self.knowledge_retriever.prove(formula, previous_substitution=previous_substitution, knowledge_base=self)
        ]

        if not retrieve_only:
            proof_sources.extend(
                prover.prove(formula, previous_substitution=previous_substitution, knowledge_base=self)
                for prover in self.get_provers_for(formula)
            )

        # TODO make buffer_size configurable
        proof_process = asynctools.multiplex(*proof_sources, buffer_size=1)

        return proof_process

    def ponder(self, *formulas: Iterable[LogicObject], ponder_mode: PonderMode):
        proving_process = self.__make_proving_process(formulas, ponder_mode)

        # TODO make buffer_size configurable
        for proof in self._scheduler.schedule_generator(
                process_with_loopback(input_sequence=proving_process, processor=self.__ponder_single_proof),
                buffer_size=1
        ):
            yield proof

    def __make_proving_process(self, formulas, ponder_mode):
        if ponder_mode == PonderMode.HYPOTHETICALLY:
            raise NotImplementedError("This case requires hypotheses to be implemented :P")
        elif ponder_mode == PonderMode.KNOWN:
            # TODO make buffer_size configurable
            proving_process = asynctools.multiplex(
                *(self.async_prove(f, retrieve_only=True) for f in formulas),
                buffer_size=1
            )
        elif ponder_mode == PonderMode.PROVE:
            # TODO make buffer_size configurable
            proving_process = asynctools.multiplex(
                *(self.async_prove(f, retrieve_only=False) for f in formulas),
                buffer_size=1
            )
        else:
            raise NotImplementedError(f"Unknown ponder mode: {ponder_mode}")
        return proving_process

    async def __ponder_single_proof(self, proof: Proof, *, queue: asyncio.Queue, poison_pill):
        pondering_sources = []
        for listener in self.get_listeners_for(proof.conclusion):
            trigger_premise = Proof(
                inference_rule=TriggeringFormula(),
                conclusion=proof.substitution.apply_to(proof.conclusion),
                substitution=proof.substitution,
                premises=(proof,)
            )
            pondering_sources.append(listener.ponder(trigger_premise, knowledge_base=self))

        # TODO make buffer_size configurable
        pondering_process = asynctools.multiplex(
            *pondering_sources,
            buffer_size=1
        )

        await asynctools.push_each_to_queue(pondering_process, queue=queue, poison_pill=poison_pill)

    def get_listeners_for(self, formula):
        key = make_key(formula)
        yield from self._listener_storage.retrieve(key)

    def get_provers_for(self, formula):
        key = make_key(formula)
        yield from self._prover_storage.retrieve(key)

    def is_hypothetical(self) -> bool:
        # TODO: when hypotheses exist, this must be done
        return False



