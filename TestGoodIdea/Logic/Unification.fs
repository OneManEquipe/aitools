module Tests.Logic.Unification

open AITools.Logic.Language
open AITools.Logic.Unification
open AITools.Logic.Utils
open AITools.Logic.Core
open Xunit

[<Fact>]
let ``Headed binding representation works correctly`` () =
    let language = Language()
    let x = makeNamed language Variable "x"
    let y = makeNamed language Variable "y"
    let a = makeNamed language Constant "a"
    let b = makeNamed language Constant "b"

    let e = makeExpr [ a; b ]

    let binding = Binding(set [ x; y ], Some e)

    Assert.Equal("{?x1, ?y2 -> (a3, b4)}", string binding)

[<Fact>]
let ``Headless binding representation works correctly`` () =
    let language = Language()
    let x = makeNamed language Variable "x"
    let y = makeNamed language Variable "y"

    let binding = Binding(set [ x; y ], None)

    Assert.Equal("{?x1, ?y2 -> _}", string binding)


[<Fact>]
let ``Substitution representation works correctly`` () =
    let language = Language()

    let x = makeNamed language Variable "x"
    let y = makeNamed language Variable "y"
    let z = makeNamed language Variable "z"

    let a = makeNamed language ConstExpr "a"

    let subst =
        Substitution
            ([ Binding(set [ x; y ], Some a)
               Binding(set [ z ], None) ])

    Assert.Equal("[{?x1, ?y2 -> a4}, {?z3 -> _}]", string subst)

[<Fact>]
let ``Recursive substitutions are impossible`` () =
    let language = Language()

    let x = makeNamed language Variable "x"
    let a = makeNamed language Constant "a"

    let e = makeExpr [ x; a ]

    Assert.Throws<System.ArgumentException>(fun () -> Substitution([ Binding(set [ x ], Some e) ]) :> obj)
    |> ignore

[<Fact>]
let ``Substitutions are applied recursively`` () =
    let language = Language()

    let x = makeNamed language Variable "x"
    let y = makeNamed language Variable "y"

    let a = makeNamed language ConstExpr "a"
    let b = makeNamed language ConstExpr "b"
    let c = makeNamed language ConstExpr "c"

    let s =
        Substitution
            ([ Binding(set [ y ], Some b)
               Binding(set [ x ], Some(makeExpr [ a; y ])) ])

    let expected = makeExpr [ a; b ]

    Assert.Equal(expected, s.ApplyTo(Var x))

[<Fact>]
let ``Different constants do not unify`` () =
    let language = Language()
    let a = makeNamed language ConstExpr "a"
    let b = makeNamed language ConstExpr "b"

    Assert.Equal(None, Substitution.Unify(a, b))


[<Fact>]
let ``Equal expressions unify`` () =
    let language = Language()
    let a = makeNamed language ConstExpr "a"
    let b = makeNamed language ConstExpr "b"
    let c = makeNamed language ConstExpr "c"
    let d = makeNamed language ConstExpr "d"

    let e1 = makeExpr [ a; [ b; c ]; d ]
    let e2 = makeExpr [ a; [ b; c ]; d ]

    let expected = Substitution([])

    Assert.Equal(Some expected, Substitution.Unify(e1, e2))



[<Fact>]
let ``Different expressions do not unify`` () =
    let language = Language()
    let a = makeNamed language ConstExpr "a"
    let b = makeNamed language ConstExpr "b"
    let c = makeNamed language ConstExpr "c"
    let d = makeNamed language ConstExpr "d"

    let e1 = makeExpr [ a; [ b; c ]; d ]
    let e2 = makeExpr [ a; [ b; c ]; a ]

    Assert.Equal(None, Substitution.Unify(e1, e2))


[<Fact>]
let ``Complex unifiable expression actually do unify`` () =
    let language = Language()
    let v1 = makeNamed language Variable "v1"
    let v2 = makeNamed language Variable "v2"

    let a = makeNamed language ConstExpr "a"
    let b = makeNamed language ConstExpr "b"
    let c = makeNamed language ConstExpr "c"
    let d = makeNamed language ConstExpr "d"

    let exprD = makeExpr [ d ]

    let e1 = makeExpr [ a; [ b; c ]; exprD ]

    let e2 = makeExpr [ a; [ Var v1; c ]; v2 ]

    let expected =
        Substitution
            ([ Binding(set [ v1 ], Some b)
               Binding(set [ v2 ], Some exprD) ])

    Assert.Equal(Some expected, Substitution.Unify(e1, e2))

[<Fact>]
let ``Unification fails when the same variable gets unified with incompatible expressions`` () =
    let language = Language()
    let v1 = make language Variable

    let a = makeNamed language ConstExpr "a"
    let b = makeNamed language ConstExpr "b"
    let c = makeNamed language ConstExpr "c"
    let d = makeNamed language ConstExpr "d"

    let exprD = makeExpr [ d ]

    let e1 = makeExpr [ a; [ b; c ]; exprD ]

    let e3 = makeExpr [ a; [ Var v1; c ]; v1 ]

    Assert.Equal(None, Substitution.Unify(e1, e3))

// TODO translate from Python test_unification_with_variables_success_equality
// TODO translate from Python test_unification_with_variables_failure_contained
// TODO translate from Python test_unification_with_variables_success_same_expression
// TODO translate from Python test_unification_with_previous_simple_failing
// TODO translate from Python test_unification_with_previous_success_bound_to_same_expression
// TODO translate from Python test_unification_with_previous_success_bound_to_unifiable_expressions
// TODO translate from Python test_unification_with_previous_failure_bound_to_different_expressions
// TODO translate from Python test_unification_with_previous
// TODO translate from Python test_unification_with_repeated_constants
// TODO translate from Python test_unification_weird_failing_case
