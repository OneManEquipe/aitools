from collections import deque
from typing import Set, Collection, Optional, Iterable, Union, Deque

import ZODB

from aitools.logic import Expression, Substitution
from aitools.logic.utils import renew_variables
from aitools.proofs.context import contextual
from aitools.proofs.listeners import Listener, _MultiListenerWrapper
from aitools.proofs.proof import Prover, ProofSet, Proof
from aitools.proofs.provers import KnowledgeRetriever, RestrictedModusPonens
from aitools.proofs.utils import EmbeddedProver


class ZodbPersistentKnowledgeBase:

    def __init__(self, *, storage=None):
        self.db = ZODB.DB(storage)

        self._initialize_db()

        self._provers: Set[Prover] = set()
        self._listeners: Set[Listener] = set()
        # TODO switch to a custom collection with limited capacity, to avoid infinite growth
        self._temporary_listeners: Collection[Listener] = set()

        self._initialize_default_provers()

    def _initialize_db(self):
        with self.db.transaction("initializing db if necessary") as conn:
            if 'known_formulas' not in conn.root():
                conn.root.known_formulas = set()

    def _initialize_default_provers(self):
        self.add_provers(KnowledgeRetriever())
        # although it's quite a standard proving strategy, I really don't like having MP as a default...
        self.add_provers(RestrictedModusPonens())

    def retrieve(self, formula: Optional[Expression] = None) -> Iterable[Substitution]:
        """Retrieves all formula from the KnowledgeBase which are unifiable with the given one.
        No proofs are searched, so either a formula is **IN** the KB, or nothing will be returned"""
        # TODO index! indeeeeeex! INDEEEEEEX! I N D E E E E E E E X ! ! ! ! !
        with self.db.transaction() as conn:
            for expr in conn.root.known_formulas:
                subst = Substitution.unify(expr, formula) if formula is not None else Substitution()

                if subst is not None:
                    yield subst

    def add_formulas(self, *formulas: Expression):
        """Adds all of the given formulas to the currently known formulas."""
        with self.db.transaction() as conn:
            formulas = tuple(renew_variables(f) for f in formulas)
            for f in formulas:
                if not isinstance(f, Expression):
                    raise TypeError("Only formulas can be added to a Knowledge Base!")
                conn.root.known_formulas.add(f)
            conn.root._p_changed__ = True

        for f in formulas:
            self.__on_formula_proven(f)

    def add_provers(self, *provers):
        for p in provers:
            if isinstance(p, Prover):
                self._provers.add(p)
            else:
                self._provers.add(EmbeddedProver(p.wrapped_function, p.formula))

    def add_listeners(self, *listeners: Union[Listener, _MultiListenerWrapper],
                      retroactive: bool = False, temporary=False):
        if retroactive:
            raise NotImplementedError("Not implemented yet!")

        destination = self._listeners if not temporary else self._temporary_listeners

        for el in listeners:
            if isinstance(el, Listener):
                destination.add(el)
            elif isinstance(el, _MultiListenerWrapper):
                for l in el.listeners:
                    destination.add(l)

    def prove(self, formula: Expression, truth: bool = True) -> ProofSet:

        """Backward search to prove a given formulas using all known provers"""
        proof_sources: Deque[Iterable[Proof]] = deque(
            prover(formula, _kb=self, _truth=truth) for prover in self._provers
        )

        _embedded_prover: Prover = getattr(formula, '_embedded_prover', None)
        if _embedded_prover is not None:
            proof_sources.appendleft(_embedded_prover(formula=formula, _kb=self, _truth=truth))

        @contextual('kb', self)
        def _inner():
            while any(proof_sources):
                source = proof_sources.popleft().__iter__()
                try:
                    new_proof = next(source)
                except StopIteration:
                    pass
                else:
                    proof_sources.append(source)
                    self.__on_formula_proven(new_proof.substitution.apply_to(new_proof.conclusion))
                    yield new_proof

        return ProofSet(_inner())

    def __on_formula_proven(self, formula):
        for listener in self.__get_listeners_for(formula):
            self.__process_listener(listener, formula)

        for listener in self.__get_listeners_for(formula, temporary=True):
            self.__process_listener(listener, formula)

    def __process_listener(self, listener, formula):
            raw_output = listener.extract_and_call(formula)
            if raw_output is None:
                return

            output = (raw_output,) if isinstance(raw_output, (Expression, Listener, _MultiListenerWrapper)) \
                else raw_output

            # TODO it would help to keep these separated from the actual formulas, to prevent overflowing memory
            for obj in output:
                if isinstance(obj, Expression):
                    self.add_formulas(obj)
                elif isinstance(obj, Listener):
                    self.add_listeners(obj, temporary=True)
                elif isinstance(obj, _MultiListenerWrapper):
                    self.add_listeners(*obj.listeners, temporary=True)

    def __get_listeners_for(self, formula: Expression, *, temporary=False):
        # TODO indexing (we already take the formula as input to that purpose)
        source = self._listeners if not temporary else self._temporary_listeners
        # TODO use an ordered data structure! this is like the inefficientest of inefficiencies
        for listener in sorted(source, key=lambda l: l.priority, reverse=True):
            yield listener