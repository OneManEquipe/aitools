module AITools.Logic.Unification

open System
open AITools.Logic.Core
open System.Collections.Immutable

exception UnificationFailedException of string

type Binding(variables: Set<Variable>, head: Expression option) =
    // check that none of the variables are in the head
    do
        match head with
        | Some expr when variables |> Set.exists (Var >> expr.Contains) ->
            raise
            <| System.ArgumentException("The head of a binding cannot contain any of its variables")
        | _ -> ()

    do
        if Set.isEmpty variables then
            raise
            <| System.ArgumentException("The variables of a binding cannot be the empty set")

    member this.Variables = variables
    member this.Head = head

    member this.BoundObject =
        match head with
        | Some logicObject -> logicObject
        | None -> variables |> Set.minElement |> Var

    static member Join(a: Binding, b: Binding) =
        let newHead =
            match a.Head, b.Head with
            | None, None -> None
            | Some _, None -> a.Head
            | None, Some _ -> b.Head
            | Some aHead, Some bHead ->
                let unifier = Substitution.Unify(aHead, bHead)
                match unifier with
                | Some subst -> Some <| subst.ApplyTo(aHead)
                | None ->
                    raise
                    <| UnificationFailedException("Unable to unify the heads of the two bindings!")

        let newVars = Set.union a.Variables b.Variables

        Binding(newVars, newHead)

    override this.ToString() =
        let vars =
            this.Variables
            |> Set.map string
            |> Set.toArray
            |> String.concat ", "

        let head =
            match this.Head with
            | Some expr -> string expr
            | None -> "_"

        sprintf "{%s -> %s}" vars head

    override this.GetHashCode() = hash (head, variables)

    override this.Equals(other) =
        match other with
        | :? Binding as b -> this.Head = b.Head && this.Variables = b.Variables
        | _ -> false

and Substitution(bindings: Binding seq) =
    // TODO I'm using a map here, but I might switch to a Dictionary for performance, who knows :P
    let mutable bindingsByVariable: Map<Variable, Binding> = Map.empty

    do
        for binding in bindings do
            // TODO replace this with a fold
            let mutable mergedBinding = binding
            for v in binding.Variables do
                mergedBinding <-
                    match bindingsByVariable.TryFind(v) with
                    | Some b -> Binding.Join(mergedBinding, b)
                    | None -> mergedBinding

            for v in mergedBinding.Variables do
                bindingsByVariable <- bindingsByVariable.Add(v, mergedBinding)

    member this.IsEmpty() = Map.isEmpty bindingsByVariable

    member this.BindingsByVariable = bindingsByVariable
    member this.WithBindings(otherBindings: Binding seq) =
        Seq.concat [ bindings; otherBindings ]
        |> Substitution

    member this.ApplyTo(expr) =
        match expr with
        | Var v ->
            match bindingsByVariable.TryFind(v) with
            | Some binding -> this.ApplyTo(binding.BoundObject)
            | None -> expr
        | Expr arr -> arr |> Seq.map this.ApplyTo |> ImmutableArray.CreateRange |> Expr
        | _ -> expr

    member this.GetBoundObjectFor(v) = bindingsByVariable.TryFind(v)

    static member Empty = Substitution([||])

    static member Unify(a, b, ?previous: Substitution): Substitution option =
        let previous =
            match previous with
            | Some subst -> subst
            | None -> Substitution.Empty

        let a = previous.ApplyTo(a)
        let b = previous.ApplyTo(b)

        match a, b with
        | _ when a = b -> Some previous
        | Var va, Var vb ->
            Binding(set [ va; vb ], None)
            |> Seq.singleton
            |> previous.WithBindings
            |> Some
        | Var v, _ when not (b.Contains(a)) ->
            Binding(Set.singleton v, Some b)
            |> Seq.singleton
            |> previous.WithBindings
            |> Some
        | _, Var v when not (a.Contains(b)) ->
            Binding(Set.singleton v, Some a)
            |> Seq.singleton
            |> previous.WithBindings
            |> Some
        | Expr aArr, Expr bArr when aArr.Length = bArr.Length ->
            let folder maybeSubst aElem bElem =
                match maybeSubst with
                | Some subst -> Substitution.Unify(aElem, bElem, previous = subst)
                | None -> None

            (aArr, bArr)
            ||> Seq.fold2 folder (Some Substitution.Empty)
        | _ -> None

    override this.ToString() =
        bindings
        |> Seq.map string
        |> Array.ofSeq
        |> String.concat ", "
        |> sprintf "[%s]"

    override this.GetHashCode() = hash bindingsByVariable

    override this.Equals(other) =
        match other with
        | :? Substitution as s -> this.BindingsByVariable = s.BindingsByVariable
        | _ -> false
