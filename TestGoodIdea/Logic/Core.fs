module Tests.Logic.Core

open AITools.Logic.Core
open AITools.Logic.Language
open Xunit
open AITools.Logic.Utils

[<Fact>]
let ``Named Constant representation works correctly`` () =
    let lang = Language()

    let c = makeNamed lang Constant "foo"

    Assert.Equal("foo1", string c)

[<Fact>]
let ``Unnamed Constant representation works correctly`` () =
    let lang = Language()

    let c = make lang Constant

    Assert.Equal("o1", string c)

[<Fact>]
let ``Named Variable representation works correctly`` () =
    let lang = Language()

    let v = makeNamed lang Variable "foo"

    Assert.Equal("?foo1", string v)

[<Fact>]
let ``Unnamed Variable representation works correctly`` () =
    let lang = Language()

    let v = make lang Variable

    Assert.Equal("?v1", string v)


[<Fact>]
let ``Int wrapper representation works correctly`` () =
    let w = Wrap(33)

    Assert.Equal("{33}", string w)


[<Fact>]
let ``String wrapper representation works correctly`` () =
    let w = Wrap("wow")

    Assert.Equal("{wow}", string w)


[<Fact>]
let ``Simple expression representation works correctly`` () =
    let lang = Language()
    let expr = make lang ConstExpr

    Assert.Equal("o1", string expr)

[<Fact>]
let ``Complex expression representation works correctly`` () =
    let lang = Language()

    let a = make lang ConstExpr
    let b = make lang ConstExpr
    let c = make lang ConstExpr
    let x = make lang VarExpr

    let expr = makeExpr [ a; [ b; c ]; x ]

    Assert.Equal("(o1, (o2, o3), ?v4)", string expr)


[<Fact>]
let ``Expression.Contains returns true when the expression contains a Variable`` () =
    let lang = Language()
    let element = make lang VarExpr

    let a = make lang ConstExpr
    let b = make lang ConstExpr
    let c = make lang ConstExpr
    let x = make lang VarExpr

    let expr = makeExpr [ a; [ b; element; c ]; x ]

    Assert.True(expr.Contains(element))


[<Fact>]
let ``Expression.Contains returns true when the expression contains a Constant`` () =
    let lang = Language()

    let element = make lang ConstExpr

    let a = make lang ConstExpr
    let b = make lang ConstExpr
    let c = make lang ConstExpr
    let x = make lang VarExpr

    let expr = makeExpr [ a; [ b; element; c ]; x ]

    Assert.True(expr.Contains(element))


[<Fact>]
let ``Expression.Contains returns true when the expression contains a Wrapper`` () =
    let lang = Language()
    let element = Wrap "foo"

    let a = make lang ConstExpr
    let b = make lang ConstExpr
    let c = make lang ConstExpr
    let x = make lang VarExpr

    let expr = makeExpr [ a; [ b; element; c ]; x ]

    Assert.True(expr.Contains(element))


[<Fact>]
let ``Expression.Contains returns true when the expression contains another Expression`` () =
    let lang = Language()

    let a' = make lang ConstExpr
    let b' = make lang ConstExpr
    let element = makeExpr [a'; b']


    let a = make lang ConstExpr
    let b = make lang ConstExpr
    let c = make lang ConstExpr
    let x = make lang VarExpr

    let expr = makeExpr [ a; [ b; element; c ]; x ]

    Assert.True(expr.Contains(element))
