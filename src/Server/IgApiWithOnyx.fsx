#r "nuget: Oryx"
#r "nuget: Oryx.ThothJsonNet"

#load "Credentials.fs"
open Credentials

let api = apiKey

//open System.Net.Http
open System.Text.Json

//open FSharp.Control.Tasks

// open Oryx.SystemTextJson.ResponseReader
open Oryx.ThothJsonNet.ResponseReader

open Oryx

[<Literal>]
let Url = "https://en.wikipedia.org/w/api.php"

let options = JsonSerializerOptions()

let query term = [
    struct ("action", "opensearch")
    struct ("search", term)
]

let request term =
    GET
    >=> withUrl Url
    >=> withQuery (query term)
    >=> fetch
    >=> json options