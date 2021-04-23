#r "nuget: Oryx"
#r "nuget: Oryx.ThothJsonNet"

open System.Net.Http
open Thoth.Json.Net
open Oryx
open Oryx.ThothJsonNet
open Oryx.ThothJsonNet.ResponseReader

#load "Credentials.fs"
open Credentials

let credentials = Demo.alpacaAuth


type AuthId = AuthId of string
type AccountNo = AccountNo of string

type AuthResponse = {
    AuthId: AuthId
    AccountNo: AccountNo
}
with
    static member Decoder: Decoder<AuthResponse> = 
        Decode.object (fun get ->
            let authId = get.Required.At ["id"] Decode.string
            let accountNo = get.Required.At ["account_number"] Decode.string
            {
                AuthId = AuthId authId
                AccountNo = AccountNo accountNo
            })

let (Url url) = credentials.Url

let headers (ApiKey apiKey) (SecretKey secretKey) = 
    [
        ("APCA-API-KEY-ID", apiKey)
        ("APCA-API-SECRET-KEY", secretKey)
        ("Accept", "application/json; charset=UTF-8")
    ] 
    |> Map.ofList


let client = new HttpClient()


let context = 
    HttpContext.defaultContext
    |> HttpContext.withHttpClient client
    |> HttpContext.withHeaders (headers credentials.Key credentials.SecretKey)


let request<'a> =
    GET
    >=> withUrl url
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
// "Response: { AuthId    = AuthId "xxx"
//              AccountNo = AccountNo "yyy" }"