module AITools.Logic.Language

open System.Threading


type Language(languageId, enabled) =
    let nextId = ref 0L

    member this.LanguageId = languageId
    member this.GetNext() = if enabled then Interlocked.Increment(nextId) else failwith "Disabled languags cannot generate ids anymore"

    new() = Language(System.Guid.NewGuid(), true)

    override this.ToString() = languageId |> sprintf "Language(%A)"
    
    override this.GetHashCode() = hash languageId
    override this.Equals(other) = 
        match other with
        | :? Language as l -> languageId = l.LanguageId
        | _ -> false

    interface System.IComparable with
        member x.CompareTo y = 
            match y with
            | :? Language as y -> x.LanguageId.CompareTo(y.LanguageId)
            | _ -> raise <| System.ArgumentException("Cannot compare instances of different types")