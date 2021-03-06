from __future__ import annotations

import itertools
from dataclasses import dataclass
from enum import Enum, auto
from typing import Iterable, Collection, Union

import typing

from aitools.logic.core import LogicObject
from aitools.logic.unification import Substitution
from aitools.logic.utils import normalize_variables
from aitools.proofs.components import HandlerSafety, Component
from aitools.proofs.exceptions import UnsafeOperationException
from aitools.proofs.provers import Proof
from aitools.utils import asynctools


class PonderMode(Enum):
    KNOWN = auto()
    PROVE = auto()
    HYPOTHETICALLY = auto()


@dataclass(frozen=True)
class TriggeringFormula:
    pass


@dataclass
class FormulaSubstitution:
    formula: LogicObject
    substitution: Substitution


@dataclass
class FormulaSubstitutionPremises:
    formula: LogicObject
    substitution: Substitution
    premises: Union[Proof, Collection[Proof]]


@dataclass
class Pondering:
    listener: Listener
    triggering_formula: LogicObject


class Listener(Component):

    async def ponder(self, proof: Proof, knowledge_base) -> typing.AsyncIterable[Proof]:
        formula = proof.conclusion

        if knowledge_base.is_hypothetical() and self.safety == HandlerSafety.TOTALLY_UNSAFE:
            raise UnsafeOperationException("Unsafe listener cannot be used in hypothetical scenarios")

        normalized_listened_formula, normalization_mapping = normalize_variables(self.listened_formula,
                                                                                 language=self._language)
        unifier = Substitution.unify(formula, normalized_listened_formula,
                                     previous=proof.substitution)

        if unifier is None:
            return

        try:
            args_by_name = self._extract_args_by_name(formula=formula, unifier=unifier,
                                                      knowledge_base=knowledge_base,
                                                      normalization_mapping=normalization_mapping)
        except ValueError:
            return

        result = self.handler(**args_by_name)

        if isinstance(result, typing.Coroutine):
            result = await result

        if result is None:
            return

        # if the handler returned a single value, wrap it in a tuple to make it iterable
        if (
                isinstance(result, (LogicObject, FormulaSubstitution, FormulaSubstitutionPremises)) or
                # the result is a pair or a triple, but not "made of results
                (
                        isinstance(result, tuple) and
                        len(result) in (2, 3) and
                        not all(isinstance(x, LogicObject) for x in result)
                )
        ):
            result = asynctools.wrap_item_in_async_generator(result)

        if isinstance(result, Iterable):
            result = asynctools.asynchronize(result)

        async for item in result:
            if isinstance(item, tuple):
                if len(item) == 2:
                    conclusion, substitution = item
                    premises = ()
                elif len(item) == 3:
                    conclusion, substitution, premises = item
                    # again, if a single proof was returned, we wrap it in a tuple to make it iterable
                    if isinstance(premises, Proof):
                        premises = (premises,)
                else:
                    raise ValueError("A handler returned tuple must have length 2 or 3")
            elif isinstance(item, LogicObject):
                conclusion = item
                substitution = unifier
                premises = ()
            elif isinstance(item, FormulaSubstitution):
                conclusion = item.formula
                substitution = item.substitution
                premises = ()
            elif isinstance(item, FormulaSubstitutionPremises):
                conclusion = item.formula
                substitution = item.substitution
                premises = (item.premises,) if isinstance(item.premises, Proof) else item.premises
            else:
                raise ValueError(
                    f"A Listener's handler cannot return a {type(item)}, read the docs! LogicObjects, "
                    f"(formula, substitution) pairs, (formula, substitution, Proof/Proofs) triples, "
                    f"FormulaSubstitution instances or FormulaSubstitutionProofs instances! "
                    f"What is a {type(item)}????"
                )

            proof = Proof(
                inference_rule=Pondering(listener=self, triggering_formula=formula),
                conclusion=conclusion,
                substitution=substitution,
                premises=tuple(itertools.chain((proof,), premises))
            )

            yield proof
