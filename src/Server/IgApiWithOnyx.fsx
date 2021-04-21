#r "nuget: Oryx"
#r "nuget: Oryx.ThothJsonNet"

#load "Credentials.fs"
open Credentials
open Thoth.Json.Net
open Oryx
open Oryx.ThothJsonNet
open System.Net.Http

let credentials = Demo.igApi



[<Literal>]
let Url = "https://demo-api.ig.com/gateway/deal/session/"


let headers (ApiKey apiKey) (AccountId id) = 
    [
        ("VERSION", "3")
        ("IG-ACCOUNT-ID", id)
        ("X-IG-API-KEY", apiKey)
       // ("Content-Type", "application/json; charset=UTF-8")
        ("Accept", "application/json; charset=UTF-8")
    ] 
    |> Map.ofList


let client = new HttpClient()

let context = 
    HttpContext.defaultContext
    |> HttpContext.withHttpClient client
    |> HttpContext.withHeaders (headers credentials.Key credentials.Id)



let encodeBody (Identifier identifier) (Password password) = 
    Encode.object [
        "identifier", Encode.string identifier
        "password", Encode.string password
    ]

let contentBuilder = 
    (fun () -> 
        new JsonPushStreamContent(encodeBody credentials.Identifier credentials.Password) 
        :> HttpContent)


let request<'a> =
    POST
    >=> withUrl Url
    >=> withContent contentBuilder
    >=> fetch

let resultTask = 
    request
    |> runAsync context

let resultToString response = 
    async {
        let! r = response
        match r with 
        | Ok result -> return sprintf $"Response: {result}"
        | Error msg -> return sprintf $"Error: {msg}"
    }

let authResponse = 
    resultTask
    |> Async.AwaitTask
    |> resultToString
    |> Async.RunSynchronously

// "Response: System.Net.Http.HttpConnectionResponseContent"
