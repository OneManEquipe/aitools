module AITools.Storage.Index

open AITools.Logic.Core
open System.Collections.Immutable


type KeyElement<'a> =
    | Wildcard
    | Further of int
    | Literal of 'a

    override this.ToString() =
        match this with
        | Wildcard -> "*"
        | Further n -> string n
        | Literal x -> x.ToString()

type KeySlice<'a> =
    { Elements: KeyElement<'a> ImmutableArray }

    override this.ToString() =
        this.Elements
        |> Seq.map string
        |> String.concat ", "
        |> sprintf "[%s]"

type Key<'a> =
    { Slices: KeySlice<'a> ImmutableArray }
    override this.ToString() =
        this.Slices
        |> Seq.map string
        |> String.concat "\n"


let makeKey expr =
    let tempRes =
        ResizeArray<ImmutableArray.Builder<Expression KeyElement>>()

    let rec inner (expr, level) =
        if tempRes.Count = level
        then tempRes.Add(ImmutableArray.CreateBuilder<Expression KeyElement>())

        match expr with
        | Expr children ->
            tempRes.[level].Add(Further children.Length)
            for child in children do
                inner (child, level + 1)
        | Var _ -> tempRes.[level].Add(Wildcard)
        | _ -> tempRes.[level].Add(Literal expr)

    inner (expr, 0)

    { Slices =
          ImmutableArray.CreateRange
              (seq {
                  for slice in tempRes do
                      yield { Elements = slice.ToImmutable() }
               }) }


let extendKeySlice (slice: KeySlice<'a> option) (element: KeyElement<'a>) =
    match slice with
    | None -> { Elements = ImmutableArray.CreateRange(seq { element }) }
    | Some { Elements = els } ->
        { Elements =
              ImmutableArray.CreateRange
                  (seq {
                      yield! els
                      yield element
                   }) }

// TODO rename to projectSlice since it operates on slices
let projectSlice (previousSlice: KeySlice<'a>) (projectionSlice: KeySlice<'a>) (currentSlice: KeySlice<'a>) =
    let mutable iCurrent = 0

    let res = ResizeArray<KeyElement<'a>>()

    projectionSlice.Elements
    |> Seq.iteri (fun i projector ->
        match projector with
        | Further n ->
            match previousSlice.Elements.[i] with
            | Wildcard ->
                res.AddRange
                    (seq {
                        for _ = 1 to n do
                            Wildcard
                     })
            | _ ->
                res.AddRange
                    (seq {
                        for i = iCurrent to iCurrent + n - 1 do
                            currentSlice.Elements.[i]
                     })
                iCurrent <- iCurrent + n
        | _ -> ())

    res.AddRange
        (seq {
            for i = iCurrent to currentSlice.Elements.Length - 1 do
                currentSlice.Elements.[i]
         })

    { Elements = res.ToImmutableArray() }

[<AbstractClass>]
type TrieIndex<'keyItem, 'item>() =

    member this.Add(key: KeySlice<'keyItem>, item: 'item) = this.Add(key, item, 0)

    member this.Retrieve(key, ?useWildcard) =
        seq {
            let useWildcard = defaultArg useWildcard true
            for r in this.Retrieve(key, level = 0, useWildcard = useWildcard, foundKey = None) do
                yield r
        }

    member private this.Add(key, item, level) =
        if level = key.Elements.Length then
            this.MaybeStoreObject(item)
        else
            let element = key.Elements.[level]
            let subIndex = this.GetOrCreateSubindex(element)
            subIndex.Add(key, item, level + 1)

    member private this.Retrieve(key, level, useWildcard, foundKey: KeySlice<'keyItem> option) =
        seq {
            match key with
            | None ->

                for item in this.Objects do
                    yield item, foundKey
                for subIndex in this.Subindices do
                    yield! subIndex.Retrieve(key, level + 1, useWildcard, foundKey)

            | Some k ->
                if level = k.Elements.Length then
                    for item in this.Objects do
                        yield item, foundKey
                else
                    yield! this.TraverseNextKeyElement(k, level, useWildcard, foundKey)
        }

    member private this.TraverseNextKeyElement(key, level, useWildcard, foundKey) =
        seq {
            let keyElement = key.Elements.[level]
            match keyElement with
            | Wildcard when useWildcard -> yield! this.SearchWildcard(key, level, useWildcard, foundKey)
            | Wildcard -> yield! this.SearchForKeyElementExplicitly(key, Wildcard, level, useWildcard, foundKey)
            | _ ->
                yield! this.SearchForKeyElementExplicitly(key, keyElement, level, useWildcard, foundKey)
                yield! this.SearchForVariable(key, level, useWildcard, foundKey)
        }

    member private this.SearchWildcard(key, level, useWildcard, foundKey) =
        seq {
            for subKeyElement, subindex in this.KeysAndSubindices do
                let newFoundKey =
                    Some <| extendKeySlice foundKey subKeyElement

                yield! subindex.Retrieve(Some key, level + 1, useWildcard, newFoundKey)
        }

    member private this.SearchForVariable(key, level, useWildcard, foundKey) =
        seq {
            if useWildcard then
                match this.GetSubindexByKeyElement(Wildcard) with
                | Some subIndex ->
                    let newFoundKey = Some <| extendKeySlice foundKey Wildcard
                    yield! subIndex.Retrieve(Some key, level + 1, useWildcard, newFoundKey)
                | None -> ()
        }

    member private this.SearchForKeyElementExplicitly(key, keyElement, level, useWildcard, foundKey) =
        seq {
            match this.GetSubindexByKeyElement(keyElement) with
            | Some subIndex ->
                let newFoundKey =
                    Some <| extendKeySlice foundKey keyElement

                yield! subIndex.Retrieve(Some key, level + 1, useWildcard, newFoundKey)
            | None -> ()
        }

    abstract MaybeStoreObject: 'item -> unit
    abstract GetOrCreateSubindex: KeyElement<'keyItem> -> TrieIndex<'keyItem, 'item>
    abstract Objects: seq<'item>
    abstract Subindices: seq<TrieIndex<'keyItem, 'item>>
    abstract KeysAndSubindices: seq<KeyElement<'keyItem> * TrieIndex<'keyItem, 'item>>
    abstract GetSubindexByKeyElement: KeyElement<'keyItem> -> TrieIndex<'keyItem, 'item> option

[<AbstractClass>]
type AbstruseIndex<'keyItem, 'item, 'subindexItem when 'subindexItem :> AbstruseIndex<'keyItem, 'item, 'subindexItem>>() =
    member this.Add(key, element) = this.Add(key, element, level = 0)
    member this.Retrieve(key) = this.Retrieve(key, level = 0, previousSlice = None, projectionSlice = None)

    member private this.Add(key, element, level) =
        let slice =
            if level < key.Slices.Length then Some key.Slices.[level] else None

        match slice with
        | None -> this.MaybeStoreObject(element)
        | Some slice ->
            let furtherAbstrusion =
                Array.ofSeq
                <| this.SubindexTree.Retrieve(Some slice, useWildcard = false)
            // TODO I should not use Seq.isEmpty, since it reads the first element of the sequence (so it would take more time than necessary)

            let destination =
                if furtherAbstrusion.Length = 0 then
                    let dest = this.MakeNode()
                    this.SubindexTree.Add(slice, dest)
                    dest
                else
                    match Seq.tryExactlyOne furtherAbstrusion with
                    | Some (dest, _) -> dest
                    | None -> failwith "Do I even know what I'm doing?"

            destination.Add(key, element, level + 1)

    member private this.Retrieve(fullKey, level, previousSlice, projectionSlice) =
        seq {
            let slice =
                if level < fullKey.Slices.Length then Some fullKey.Slices.[level] else None

            yield! this.Objects

            match slice with
            | None -> yield! this.FullSearch(fullKey, previousSlice = previousSlice, level = level + 1)
            | Some slice ->
                let slice =
                    match projectionSlice, previousSlice with
                    | Some projectionSlice, Some previousSlice -> projectSlice previousSlice projectionSlice slice
                    | _ -> slice

                for subindex, foundSlice in this.SubindexTree.Retrieve(Some slice) do
                    yield! subindex.Retrieve
                               (fullKey = fullKey, level = level + 1, previousSlice = Some slice, projectionSlice = foundSlice)
        }

    member private this.FullSearch(fullKey, previousSlice, level) =
        seq {
            for subindex, foundSlice in this.SubindexTree.Retrieve(None) do
                yield! subindex.Retrieve
                           (fullKey = fullKey, level = level + 1, previousSlice = previousSlice, projectionSlice = foundSlice)
        }

    abstract SubindexTree: TrieIndex<'keyItem, 'subindexItem>
    abstract MakeNode: unit -> 'subindexItem
    abstract Objects: 'item seq
    abstract MaybeStoreObject: 'item -> unit
