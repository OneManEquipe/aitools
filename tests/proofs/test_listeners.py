from typing import List

import pytest

from aitools.logic import Substitution, LogicWrapper, LogicObject, Variable
from aitools.logic.utils import VariableSource, constants
from aitools.proofs.language import Implies
from aitools.proofs.listeners import Listener, HandlerArgumentMode, HandlerSafety, PonderMode, TriggeringFormula
from aitools.proofs.proof import Proof
from aitools.proofs.provers import KnowledgeRetriever


def test_listener_with_just_side_effects(test_knowledge_base):
    v = VariableSource()

    Is, Meows, cat, dylan = constants('Is, Meows, cat, dylan')

    calls = []

    def cats_meow(cat):
        calls.append(cat)

    listened_formula = Is(v.cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=cats_meow, argument_mode=HandlerArgumentMode.MAP,
                         pure=True, safety=HandlerSafety.TOTALLY_UNSAFE)

    test_knowledge_base.add_listener(_listener)

    test_knowledge_base.add_formulas(Is(dylan, cat))

    proofs: List[Proof] = list(test_knowledge_base.ponder(Is(dylan, cat), ponder_mode=PonderMode.KNOWN))

    assert len(proofs) == 0

    assert calls == [dylan]


def test_listener_single_result(test_knowledge_base):
    v = VariableSource()

    Is, Meows, cat, dylan = constants('Is, Meows, cat, dylan')

    def cats_meow(cat):
        return Meows(cat)

    listened_formula = Is(v.cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=cats_meow, argument_mode=HandlerArgumentMode.MAP,
                         pure=True, safety=HandlerSafety.SAFE)

    test_knowledge_base.add_listener(_listener)

    test_knowledge_base.add_formulas(Is(dylan, cat))

    proofs: List[Proof] = list(test_knowledge_base.ponder(Is(dylan, cat), ponder_mode=PonderMode.KNOWN))

    assert all(isinstance(p, Proof) for p in proofs)
    assert len(proofs) == 1

    proof: Proof = proofs.pop()

    assert proof.conclusion == Meows(dylan)
    assert set(p.conclusion for p in proof.premises) == {Is(dylan, cat)}
    assert all(isinstance(p.inference_rule, TriggeringFormula) for p in proof.premises)


def test_multiple_listeners_are_used(test_knowledge_base):
    v = VariableSource()

    Is, Meows, Purrs, cat, dylan = constants('Is, Meows, Purrs, cat, dylan')

    def cats_meow(cat):
        return Meows(cat)

    def cats_purr(cat):
        return Purrs(cat)

    listened_formula = Is(v.cat, cat)
    meow_listener = Listener(listened_formula=listened_formula, handler=cats_meow,
                             argument_mode=HandlerArgumentMode.MAP, pure=True, safety=HandlerSafety.SAFE)

    purr_listener = Listener(listened_formula=listened_formula, handler=cats_purr,
                             argument_mode=HandlerArgumentMode.MAP, pure=True, safety=HandlerSafety.SAFE)

    test_knowledge_base.add_listener(meow_listener)
    test_knowledge_base.add_listener(purr_listener)

    assert set(test_knowledge_base.get_listeners_for(listened_formula)) == {meow_listener, purr_listener}

    test_knowledge_base.add_formulas(Is(dylan, cat))

    proofs: List[Proof] = list(test_knowledge_base.ponder(Is(dylan, cat), ponder_mode=PonderMode.KNOWN))

    assert set(p.conclusion for p in proofs) == {Meows(dylan), Purrs(dylan)}


def test_single_listener_added_multiple_times(test_knowledge_base):
    v = VariableSource()

    Is, Meows, cat, dylan = constants('Is, Meows, cat, dylan')

    def cats_meow(cat):
        return Meows(cat)

    listened_formula = Is(v.cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=cats_meow, argument_mode=HandlerArgumentMode.MAP,
                         pure=True, safety=HandlerSafety.SAFE)

    test_knowledge_base.add_listener(_listener)
    test_knowledge_base.add_listener(_listener)

    assert list(test_knowledge_base.get_listeners_for(listened_formula)) == [_listener]


@pytest.mark.parametrize(['mode'], [[PonderMode.KNOWN], [PonderMode.PROVE]])
def test_listener_multiple_inputs(test_knowledge_base, mode):
    v = VariableSource()

    Is, Meows, cat, dylan, hugo = constants('Is, Meows, cat, dylan, hugo')

    def cats_meow(cat):
        return Meows(cat)

    listened_formula = Is(v.cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=cats_meow, argument_mode=HandlerArgumentMode.MAP,
                         pure=True, safety=HandlerSafety.SAFE)

    test_knowledge_base.add_listener(_listener)

    test_knowledge_base.add_formulas(
        Is(dylan, cat),
        Is(hugo, cat)
    )

    proofs: List[Proof] = list(test_knowledge_base.ponder(
        Is(dylan, cat),
        Is(hugo, cat),
        ponder_mode=mode)
    )

    assert set(p.conclusion for p in proofs) == {Meows(dylan), Meows(hugo)}


def test_listener_known_requires_formula_in_kb(test_knowledge_base):
    v = VariableSource()

    Is, Meows, cat, dylan = constants('Is, Meows, cat, dylan')

    def cats_meow(cat):
        yield Meows(cat)

    listened_formula = Is(v.cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=cats_meow, argument_mode=HandlerArgumentMode.MAP,
                         pure=True, safety=HandlerSafety.SAFE)

    test_knowledge_base.add_listener(_listener)

    proofs: List[Proof] = list(test_knowledge_base.ponder(Is(dylan, cat), ponder_mode=PonderMode.KNOWN))

    assert len(proofs) == 0


def test_listener_multiple_formulas_returned(test_knowledge_base):
    v = VariableSource()
    Is, Meows, Purrs, cat, dylan = constants('Is, Meows, Purrs, cat, dylan')

    def deduce_meow_and_purr(_x):
        return Meows(_x), Purrs(_x)

    listener = Listener(listened_formula=Is(v._x, cat), handler=deduce_meow_and_purr,
                        argument_mode=HandlerArgumentMode.MAP, pure=True, safety=HandlerSafety.SAFE)

    test_knowledge_base.add_listener(listener)
    test_knowledge_base.add_formulas(Is(dylan, cat))

    proofs: List[Proof] = list(test_knowledge_base.ponder(Is(dylan, cat), ponder_mode=PonderMode.KNOWN))

    assert set(p.conclusion for p in proofs) == {Meows(dylan), Purrs(dylan)}


def test_listener_multiple_formulas_yielded(test_knowledge_base):
    v = VariableSource()
    Is, Meows, Purrs, cat, dylan = constants('Is, Meows, Purrs, cat, dylan')

    def deduce_meow_and_purr(_x):
        yield Meows(_x)
        yield Purrs(_x)

    listener = Listener(listened_formula=Is(v._x, cat), handler=deduce_meow_and_purr,
                        argument_mode=HandlerArgumentMode.MAP, pure=True, safety=HandlerSafety.SAFE)

    test_knowledge_base.add_listener(listener)
    test_knowledge_base.add_formulas(Is(dylan, cat))

    proofs: List[Proof] = list(test_knowledge_base.ponder(Is(dylan, cat), ponder_mode=PonderMode.KNOWN))

    assert set(p.conclusion for p in proofs) == {Meows(dylan), Purrs(dylan)}


def test_single_result_with_single_premise(test_knowledge_base):
    v = VariableSource()
    Is, Meows, SomeDumbTruth, cat, dylan = constants('Is, Meows, SomeDumbTruth, cat, dylan')

    def deduce_meow_and_purr(_x):
        from aitools.proofs.context import prove
        proofs = list(prove(SomeDumbTruth))
        return Meows(_x), proofs[0]

    listener = Listener(listened_formula=Is(v._x, cat), handler=deduce_meow_and_purr,
                        argument_mode=HandlerArgumentMode.MAP, pure=True, safety=HandlerSafety.SAFE)

    test_knowledge_base.add_listener(listener)
    test_knowledge_base.add_formulas(
        SomeDumbTruth,
        Is(dylan, cat)
    )

    proofs: List[Proof] = list(test_knowledge_base.ponder(Is(dylan, cat), ponder_mode=PonderMode.KNOWN))

    assert set(p.conclusion for p in proofs) == {Meows(dylan)}
    assert set(p.conclusion for p in proofs[0].premises) == {SomeDumbTruth, Is(dylan, cat)}
    assert set(type(p.inference_rule) for p in proofs[0].premises) == {TriggeringFormula, KnowledgeRetriever}


def test_multiple_results_with_multiple_premises(test_knowledge_base):
    v = VariableSource()
    Is, Meows, SomeDumbTruth, SomeOtherDumbTruth, cat, dylan = constants(
        'Is, Meows, SomeDumbTruth, SomeOtherDumbTruth, cat, dylan'
    )

    def deduce_meow_and_purr(_x):
        from aitools.proofs.context import prove
        for proof in prove(SomeDumbTruth):
            for other_proof in prove(SomeOtherDumbTruth):
                yield Meows(_x), (proof, other_proof)

    listener = Listener(listened_formula=Is(v._x, cat), handler=deduce_meow_and_purr,
                        argument_mode=HandlerArgumentMode.MAP, pure=True, safety=HandlerSafety.SAFE)

    test_knowledge_base.add_listener(listener)
    test_knowledge_base.add_formulas(
        SomeDumbTruth,
        SomeOtherDumbTruth,
        Is(dylan, cat)
    )

    proofs: List[Proof] = list(test_knowledge_base.ponder(Is(dylan, cat), ponder_mode=PonderMode.KNOWN))

    assert set(p.conclusion for p in proofs) == {Meows(dylan)}
    assert set(p.conclusion for p in proofs[0].premises) == {SomeDumbTruth, SomeOtherDumbTruth, Is(dylan, cat)}
    assert set(type(p.inference_rule) for p in proofs[0].premises) == {TriggeringFormula, KnowledgeRetriever}
    assert sum(1 for p in proofs[0].premises if isinstance(p.inference_rule, TriggeringFormula)) == 1
    assert sum(1 for p in proofs[0].premises if isinstance(p.inference_rule, KnowledgeRetriever)) == 2


def test_listener_chain(test_knowledge_base):
    v = VariableSource()
    A, B, C, D, foo = constants('A, B, C, D, foo')

    def deduce_from_a_b(_x):
        return B(_x)

    def deduce_from_b_c(_x):
        return C(_x)

    def deduce_from_c_d(_x):
        return D(_x)

    test_knowledge_base.add_listener(Listener(
        listened_formula=B(v._x), handler=deduce_from_b_c, argument_mode=HandlerArgumentMode.MAP, pure=True,
        safety=HandlerSafety.SAFE))

    test_knowledge_base.add_listener(Listener(
        listened_formula=A(v._x), handler=deduce_from_a_b, argument_mode=HandlerArgumentMode.MAP, pure=True,
        safety=HandlerSafety.SAFE))

    test_knowledge_base.add_listener(Listener(
        listened_formula=C(v._x), handler=deduce_from_c_d, argument_mode=HandlerArgumentMode.MAP, pure=True,
        safety=HandlerSafety.SAFE))

    test_knowledge_base.add_formulas(A(foo))

    proofs = list(test_knowledge_base.ponder(A(foo), ponder_mode=PonderMode.KNOWN))

    # order is part of the assertion here
    assert list(p.conclusion for p in proofs) == [B(foo), C(foo), D(foo)]


def test_trigger_with_open_formula__known(test_knowledge_base):
    v = VariableSource()

    Is, Meows, cat, kitten, dylan, hugo, kitty = constants('Is, Meows, cat, kitten, dylan, hugo, kitty')

    def cats_meow(cat):
        yield Meows(cat)

    listened_formula = Is(v.cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=cats_meow, argument_mode=HandlerArgumentMode.MAP,
                         pure=True, safety=HandlerSafety.SAFE)

    test_knowledge_base.add_listener(_listener)

    test_knowledge_base.add_formulas(
        Is(v.x, kitten) << Implies >> Is(v.x, cat),
        Is(dylan, cat),
        Is(hugo, cat),
        Is(kitty, kitten)
    )

    proofs: List[Proof] = list(test_knowledge_base.ponder(Is(v.x, cat), ponder_mode=PonderMode.KNOWN))

    assert set(p.conclusion for p in proofs) == {Meows(dylan), Meows(hugo)}


def test_trigger_with_open_formula__prove(test_knowledge_base):
    v = VariableSource()

    Is, Meows, cat, kitten, dylan, hugo, kitty = constants('Is, Meows, cat, kitten, dylan, hugo, kitty')

    def cats_meow(cat):
        yield Meows(cat)

    listened_formula = Is(v.cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=cats_meow, argument_mode=HandlerArgumentMode.MAP,
                         pure=True, safety=HandlerSafety.SAFE)

    test_knowledge_base.add_listener(_listener)

    test_knowledge_base.add_formulas(
        Is(v.x, kitten) << Implies >> Is(v.x, cat),
        Is(dylan, cat),
        Is(hugo, cat),
        Is(kitty, kitten)
    )

    proofs: List[Proof] = list(test_knowledge_base.ponder(Is(v.x, cat), ponder_mode=PonderMode.PROVE))

    assert set(p.conclusion for p in proofs) == {Meows(dylan), Meows(hugo), Meows(kitty)}

    kitty_cat_proofs = list(p for p in proofs if p.conclusion == Meows(kitty))

    assert set(premise.conclusion for premise in kitty_cat_proofs[0].premises) == {Is(kitty, cat)}


def test_listener_exceptions_make_the_whole_thing_fail(test_knowledge_base):
    class SomeException(Exception):
        pass

    v = VariableSource()

    Is, Meows, cat, dylan = constants('Is, Meows, cat, dylan')

    def failing_listener(cat):
        raise SomeException(f"Oh noes I failed with {cat}")

    def succeeding_listener(cat):
        return Meows(cat)

    listened_formula = Is(v.cat, cat)
    succeeding = Listener(listened_formula=listened_formula, handler=succeeding_listener,
                          argument_mode=HandlerArgumentMode.MAP, pure=True, safety=HandlerSafety.TOTALLY_UNSAFE)
    failing = Listener(listened_formula=listened_formula, handler=failing_listener,
                       argument_mode=HandlerArgumentMode.MAP, pure=True, safety=HandlerSafety.TOTALLY_UNSAFE)

    test_knowledge_base.add_listener(succeeding)
    test_knowledge_base.add_listener(failing)

    test_knowledge_base.add_formulas(Is(dylan, cat))

    with pytest.raises(SomeException):
        list(test_knowledge_base.ponder(Is(dylan, cat), ponder_mode=PonderMode.KNOWN))


def test_listener_argument_mode_raw(test_knowledge_base):
    v = VariableSource()

    Is, Meows, cat, dylan = constants('Is, Meows, cat, dylan')

    call_args = {}

    def handler(formula, substitution):
        call_args.update(formula=formula, substitution=substitution)

    listened_formula = Is(v.cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=handler, argument_mode=HandlerArgumentMode.RAW,
                         pure=True, safety=HandlerSafety.TOTALLY_UNSAFE)

    test_knowledge_base.add_listener(_listener)

    test_knowledge_base.add_formulas(Is(dylan, cat))

    proofs: List[Proof] = list(test_knowledge_base.ponder(Is(dylan, cat), ponder_mode=PonderMode.KNOWN))

    assert len(proofs) == 0

    assert call_args == dict(
        formula=Is(dylan, cat),
        substitution=Substitution.unify(Is(v.cat, cat), Is(dylan, cat))
    )


def test_listener_argument_mode_raw_rejects_wrong_argument_names(test_knowledge_base):
    v = VariableSource()

    Is, Meows, cat, dylan = constants('Is, Meows, cat, dylan')

    call_args = {}

    def handler(john, smith):
        call_args.update(formula=john, substitution=smith)

    listened_formula = Is(v.cat, cat)
    with pytest.raises(ValueError):
        _listener = Listener(listened_formula=listened_formula, handler=handler, argument_mode=HandlerArgumentMode.RAW,
                             pure=True, safety=HandlerSafety.TOTALLY_UNSAFE)


def test_listener_argument_mode_map(test_knowledge_base):
    # come on, can you blame me? :P
    test_listener_with_just_side_effects(test_knowledge_base)


def test_listener_argument_mode_map_unwrapped(test_knowledge_base):
    v = VariableSource()

    Are, Meows, cat, dylan = constants('Are, Meows, cat, dylan')

    call_args = {}

    def handler(first_cat, second_cat):
        call_args.update(first_cat=first_cat, second_cat=second_cat)

    listened_formula = Are(v.first_cat, v.second_cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=handler,
                         argument_mode=HandlerArgumentMode.MAP_UNWRAPPED, pure=True,
                         safety=HandlerSafety.TOTALLY_UNSAFE)

    test_knowledge_base.add_listener(_listener)

    # poor cat :P
    wrapped_hugo = LogicWrapper("hugo")
    test_knowledge_base.add_formulas(Are(dylan, wrapped_hugo, cat))

    proofs: List[Proof] = list(test_knowledge_base.ponder(Are(dylan, wrapped_hugo, cat), ponder_mode=PonderMode.KNOWN))

    assert len(proofs) == 0

    assert isinstance(call_args['first_cat'], LogicObject)
    assert isinstance(call_args['second_cat'], str)
    assert call_args == dict(
        first_cat=dylan,
        second_cat="hugo"
    )


def test_listener_argument_mode_map_unwrapped_required_fails_if_input_is_not_wrapped(test_knowledge_base):
    v = VariableSource()

    Are, Meows, cat, dylan = constants('Are, Meows, cat, dylan')

    calls = []

    def handler(first_cat, second_cat):
        calls.append(dict(first_cat=first_cat, second_cat=second_cat))

    listened_formula = Are(v.first_cat, v.second_cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=handler,
                         argument_mode=HandlerArgumentMode.MAP_UNWRAPPED_REQUIRED, pure=True,
                         safety=HandlerSafety.TOTALLY_UNSAFE)

    test_knowledge_base.add_listener(_listener)

    # poor cat :P
    wrapped_hugo = LogicWrapper("hugo")
    test_knowledge_base.add_formulas(Are(dylan, wrapped_hugo, cat))

    proofs: List[Proof] = list(test_knowledge_base.ponder(Are(dylan, wrapped_hugo, cat), ponder_mode=PonderMode.KNOWN))

    assert len(proofs) == 0

    assert len(calls) == 0


def test_listener_argument_mode_map_unwrapped_required_succeeds_if_inputs_are_all_wrapped(test_knowledge_base):
    v = VariableSource()

    Are, Meows, cat = constants('Are, Meows, cat')

    call_args = {}

    def handler(first_cat, second_cat):
        call_args.update(first_cat=first_cat, second_cat=second_cat)

    listened_formula = Are(v.first_cat, v.second_cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=handler,
                         argument_mode=HandlerArgumentMode.MAP_UNWRAPPED_REQUIRED, pure=True,
                         safety=HandlerSafety.TOTALLY_UNSAFE)

    test_knowledge_base.add_listener(_listener)

    # poor cats :P
    wrapped_hugo = LogicWrapper("hugo")
    wrapped_dylan = LogicWrapper("dylan")
    test_knowledge_base.add_formulas(Are(wrapped_dylan, wrapped_hugo, cat))

    proofs: List[Proof] = list(test_knowledge_base.ponder(Are(wrapped_dylan, wrapped_hugo, cat), ponder_mode=PonderMode.KNOWN))

    assert len(proofs) == 0

    assert isinstance(call_args['first_cat'], str)
    assert isinstance(call_args['second_cat'], str)
    assert call_args == dict(
        first_cat="dylan",
        second_cat="hugo"
    )


def test_listener_argument_mode_map_no_variables_rejects_variables(test_knowledge_base):
    v = VariableSource()

    Are, Meows, cat, dylan = constants('Are, Meows, cat, dylan')

    calls = []

    def handler(first_cat, second_cat):
        calls.append(dict(first_cat=first_cat, second_cat=second_cat))

    listened_formula = Are(v.first_cat, v.second_cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=handler,
                         argument_mode=HandlerArgumentMode.MAP_NO_VARIABLES, pure=True,
                         safety=HandlerSafety.TOTALLY_UNSAFE)

    test_knowledge_base.add_listener(_listener)

    test_knowledge_base.add_formulas(Are(dylan, v.x, cat))

    proofs: List[Proof] = list(test_knowledge_base.ponder(Are(dylan, v.x, cat), ponder_mode=PonderMode.KNOWN))

    assert len(proofs) == 0

    assert len(calls) == 0


def test_listener_argument_mode_map_no_variables_succeeds_when_no_variables_are_passed(test_knowledge_base):
    v = VariableSource()

    Are, Meows, dylan, hugo, cat = constants('Are, Meows, dylan, hugo, cat')

    call_args = {}

    def handler(first_cat, second_cat):
        call_args.update(first_cat=first_cat, second_cat=second_cat)

    listened_formula = Are(v.first_cat, v.second_cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=handler,
                         argument_mode=HandlerArgumentMode.MAP_NO_VARIABLES, pure=True,
                         safety=HandlerSafety.TOTALLY_UNSAFE)

    test_knowledge_base.add_listener(_listener)

    test_knowledge_base.add_formulas(Are(dylan, hugo, cat))

    proofs: List[Proof] = list(test_knowledge_base.ponder(Are(dylan, hugo, cat), ponder_mode=PonderMode.KNOWN))

    assert len(proofs) == 0

    assert call_args == dict(
        first_cat=dylan,
        second_cat=hugo
    )


def test_listener_argument_mode_map_unwrapped_no_variables_reject_variables(test_knowledge_base):
    v = VariableSource()

    Are, Meows, cat, dylan = constants('Are, Meows, cat, dylan')

    calls = []

    def handler(first_cat, second_cat):
        calls.append(dict(first_cat=first_cat, second_cat=second_cat))

    listened_formula = Are(v.first_cat, v.second_cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=handler,
                         argument_mode=HandlerArgumentMode.MAP_UNWRAPPED_NO_VARIABLES, pure=True,
                         safety=HandlerSafety.TOTALLY_UNSAFE)

    test_knowledge_base.add_listener(_listener)

    test_knowledge_base.add_formulas(Are(dylan, v.x, cat))

    proofs: List[Proof] = list(test_knowledge_base.ponder(Are(dylan, v.x, cat), ponder_mode=PonderMode.KNOWN))

    assert len(proofs) == 0

    assert len(calls) == 0


def test_listener_argument_mode_map_unwrapped_no_variables_works_when_no_variables_are_passed(test_knowledge_base):
    v = VariableSource()

    Are, Meows, cat, dylan = constants('Are, Meows, cat, dylan')

    call_args = {}

    def handler(first_cat, second_cat):
        call_args.update(first_cat=first_cat, second_cat=second_cat)

    listened_formula = Are(v.first_cat, v.second_cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=handler,
                         argument_mode=HandlerArgumentMode.MAP_UNWRAPPED_NO_VARIABLES, pure=True,
                         safety=HandlerSafety.TOTALLY_UNSAFE)

    test_knowledge_base.add_listener(_listener)

    # poor cat :P
    wrapped_hugo = LogicWrapper("hugo")
    test_knowledge_base.add_formulas(Are(dylan, wrapped_hugo, cat))

    proofs: List[Proof] = list(test_knowledge_base.ponder(Are(dylan, wrapped_hugo, cat), ponder_mode=PonderMode.KNOWN))

    assert len(proofs) == 0

    assert isinstance(call_args['first_cat'], LogicObject)
    assert isinstance(call_args['second_cat'], str)
    assert call_args == dict(
        first_cat=dylan,
        second_cat="hugo"
    )


def test_listener_argument_mode_map_succeeds_with_variables(test_knowledge_base):
    v = VariableSource()

    Are, Meows, cat = constants('Are, Meows, cat')

    call_args = {}

    def handler(first_cat, second_cat):
        call_args.update(first_cat=first_cat, second_cat=second_cat)

    listened_formula = Are(v.first_cat, v.second_cat, cat)
    _listener = Listener(listened_formula=listened_formula, handler=handler,
                         argument_mode=HandlerArgumentMode.MAP, pure=True,
                         safety=HandlerSafety.TOTALLY_UNSAFE)

    test_knowledge_base.add_listener(_listener)

    test_knowledge_base.add_formulas(Are(v.x, v.y, cat))

    proofs: List[Proof] = list(test_knowledge_base.ponder(Are(v.x, v.y, cat), ponder_mode=PonderMode.KNOWN))

    assert len(proofs) == 0

    assert isinstance(call_args['first_cat'], Variable)
    assert isinstance(call_args['second_cat'], Variable)
    assert call_args == dict(
        first_cat=v.x,
        second_cat=v.y
    )


# TODO multi-listener
# TODO dynamic data-based listeners
# TODO dynamic set-up listeners