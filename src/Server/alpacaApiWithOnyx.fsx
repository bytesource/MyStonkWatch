#r "nuget: Oryx"
#r "nuget: Oryx.ThothJsonNet"

open System.Net.Http
open Thoth.Json.Net
open Oryx
open Oryx.ThothJsonNet
open Oryx.ThothJsonNet.ResponseReader

#load "Credentials.fs"
open Credentials

let credentials = Demo.igAuth


type AccessToken = AccessToken of string
type RefreshToken = RefreshToken of string


type AuthResponse = {
    AccessToken: AccessToken
    RefreshToken: RefreshToken
}
with
    static member Decoder: Decoder<AuthResponse> = 
        Decode.object (fun get ->
            let accessToken = get.Required.At ["oauthToken"; "access_token"] Decode.string
            let refreshToken = get.Required.At ["oauthToken"; "refresh_token"] Decode.string
            {
                AccessToken = AccessToken accessToken
                RefreshToken = RefreshToken refreshToken
            })

let (Url url) = credentials.Url

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
    >=> withUrl url
    >=> withContent contentBuilder
    >=> fetch
    >=> json AuthResponse.Decoder


let resultTask = 
    request
    |> runAsync context


let resultToStringAsync response = 
    async {
        let! r = response
        match r with 
        | Ok result -> return sprintf $"Response: {result}"
        | Error msg -> return sprintf $"Error: {msg}"
    }


let authResponse = 
    resultTask
    |> Async.AwaitTask
    |> resultToStringAsync
    |> Async.RunSynchronously

authResponse

// "Response: { AccessToken = AccessToken "access"
//              RefreshToken = RefreshToken "refresh" }"

// From the docs:
// access_token - the bearer token to be passed on subsequent API requests
// refresh_token - the refresh token used to request a new access token