module AITools.Storage.Implementations.Dummy

open AITools.Storage.Index
open System.Collections.Generic

type DummyTrieIndex<'keyItem, 'item when 'keyItem: equality>() =
    inherit TrieIndex<'keyItem, 'item>()

    let mutable subindices: Dictionary<KeyElement<'keyItem>, TrieIndex<'keyItem, 'item>> = Dictionary()
    let objects = HashSet()

    override _.MaybeStoreObject(object) = objects.Add(object) |> ignore

    override _.GetOrCreateSubindex(keyElement) =
        if not (subindices.ContainsKey(keyElement))
        then subindices.Add(keyElement, DummyTrieIndex<_, _>())

        subindices.[keyElement]


    override _.Objects = seq objects

    override _.Subindices = subindices |> Seq.map (fun kvp -> kvp.Value)

    override _.KeysAndSubindices =
        subindices
        |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))

    override _.GetSubindexByKeyElement(keyElement) = 
        if subindices.ContainsKey(keyElement) 
        then Some <| subindices.[keyElement]
        else None

type DummyAbstruseIndex<'keyItem, 'item when 'keyItem: equality>() =
    inherit AbstruseIndex<'keyItem, 'item, DummyAbstruseIndex<'keyItem, 'item>>()

    let objects = HashSet()

    let subindexTree = DummyTrieIndex<_, _>()

    override _.MakeNode() = DummyAbstruseIndex()
    override _.Objects = seq objects
    override _.MaybeStoreObject(object) = objects.Add(object) |> ignore
    override _.SubindexTree = upcast subindexTree
