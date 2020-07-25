module Tests.Logic.Language

open AITools.Logic.Language
open Xunit

[<Fact>]
let ``Language generates sequential ids`` () =
    let language = Language()

    Assert.Equal(language.GetNext(), 1L)
    Assert.Equal(language.GetNext(), 2L)
    Assert.Equal(language.GetNext(), 3L)


[<Fact>]
let ``Language equality is based on its id`` () =
    let commonGuid =
        System.Guid("00000000-0000-0000-0000-000000000042")

    let languageA =
        Language(languageId = commonGuid, enabled = true)

    let languageB =
        Language(languageId = commonGuid, enabled = true)

    // the two languages have different
    languageA.GetNext() |> ignore
    languageB.GetNext() |> ignore
    languageB.GetNext() |> ignore

    Assert.Equal(languageA, languageB)
    Assert.Equal(hash languageA, hash commonGuid)
    Assert.Equal(hash languageB, hash commonGuid)

[<Fact>]
let ``Language string representation`` () =
    let someGuid =
        System.Guid("00000000-0000-0000-0000-000000000042")

    let someLanguage =
        Language(languageId = someGuid, enabled = true)

    Assert.Equal("Language(00000000-0000-0000-0000-000000000042)", string someLanguage)