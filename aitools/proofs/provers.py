from aitools.logic import Expression
from aitools.logic.utils import variable_source as v
from aitools.proofs.language import Implies, Not
from aitools.proofs.proof import Prover, Proof


class KnowledgeRetriever(Prover):

    def __call__(self, formula: Expression, _kb=None, _truth: bool = True):
        """Proves a formula to be true if it is found in the knowledge base"""
        for subst in _kb.retrieve(formula):
            if _truth:
                yield Proof(inference_rule=self, conclusion=formula, substitution=subst)


class NegationProver(Prover):
    def __call__(self, formula: Expression, _kb=None, _truth: bool = True):
        """Proves the negation of a formula to be True/False by proving the formula to be False/True (respectively)"""
        if formula.children[0] == Not and len(formula.children) == 2:
            for proof in _kb.prove(formula.children[1], not _truth):
                yield Proof(inference_rule=self, conclusion=formula, substitution=proof.substitution, premises=(proof,))



class RestrictedModusPonens(Prover):
    """Restricted backward version of modus ponens, which won't perform recursive proof of implications.
    Also, it can only prove formulas to be True"""

    def __call__(self, formula: Expression, _kb=None, _truth: bool = True):
        if _truth and not formula.children[0] == Implies:
            rule_pattern = Implies(v._premise, formula)

            for rule_proof in _kb.prove(rule_pattern):
                premise = rule_proof.substitution.get_bound_object_for(v._premise)
                for premise_proof in _kb.prove(rule_proof.substitution.apply_to(premise)):
                    yield Proof(inference_rule=self, conclusion=formula, substitution=premise_proof.substitution,
                                premises=(rule_proof, premise_proof))
