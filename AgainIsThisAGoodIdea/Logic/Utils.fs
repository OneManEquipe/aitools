module AITools.Logic.Utils

open AITools.Logic.Core
open System.Collections.Immutable

let ConstExpr = Constant >> Const
let VarExpr = Variable >> Var

(* 
    TODO this only works for two nested levels (e.g. [a; [b]]), 
    but gives compilation error with three or more (e.g. [a; [b; [c;]]]), 
    I should probably switch to a parsing function 
*)
let rec makeExpr (expression: obj seq) =
    let converter (item: obj) =
        match item with
        | :? Expression as e -> e
        | :? Variable as v -> Var v
        | :? Constant as c -> Const c
        | :? (seq<obj>) as s -> makeExpr s
        | _ -> Wrap item

    expression
    |> Seq.map converter
    |> ImmutableArray.CreateRange
    |> Expr

let make lang symbolType =
    let identifier = Identifier(lang, lang.GetNext())
    symbolType (identifier, None)

let makeNamed lang symbolType name =
    let identifier = Identifier(lang, lang.GetNext())
    symbolType (identifier, Some name)
