#r "System.Net.Http"
#r "Newtonsoft.Json"

open System
open System.Net
open System.Linq
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System.Collections.Generic
open System.Text.RegularExpressions


let SendGitHubRequest (url: string) requestBody =
    async {
        use client = new HttpClient()

        client.DefaultRequestHeaders.UserAgent.Add(
            ProductInfoHeaderValue("Azure-Super-Function", "v1.0"))
        client.DefaultRequestHeaders.Authorization <-
            AuthenticationHeaderValue("token",
                Environment.GetEnvironmentVariable("GITHUB_CREDENTIALS"))
        use content =
            new StringContent(requestBody, Encoding.UTF8, "application/json")
        return! client.PostAsync(url, content) |> Async.AwaitTask
    } |> Async.RunSynchronously

let rec hasProp (key: string list) (from: JObject) =
    match from with
    | null -> false
    | _ ->
        let x = from.[key.Head]
        match x with
        | null -> false
        | _ ->
            match key with
            | [_] -> true
            | _::tl -> hasProp tl (x.Value<JObject>())
            | [] -> false

let rec prop<'T> (key: string list) (def: 'T) (from: JObject) =
    match from with
    | null -> def
    | _ ->
        let x = from.[key.Head]
        match x with
        | null -> def
        | _ ->
            match key with
            | [_] -> x.Value<'T>()
            | _::tl ->
                prop<'T> tl def (x.Value<JObject>())
            | [] -> def


let findAppName (body : string) : string = 
    let regex = Regex.Match(body, "/apps/(?<thing>.*?)/[cC]rashes", RegexOptions.Multiline).Groups.["thing"];
    if regex.Success then regex.Value
    else ""


let Run(payload: string, log: TraceWriter) =

    let apps = ["Dualog.Admin.App-Android", "AdminApp-Droid"; 
      "Dualog.Admin.App-iOS", "AdminApp-iOS"; 
      "Crew-Connection", "CrewConnection-iOS"; 
      "Crew-Connection-1", "CrewConnection-Droid"; ] |> Map.ofList

    let json = JObject.Parse(payload)
    let body = sprintf "%s" (prop["issue"; "body"] "" json)
    let comment = "{ \"body\": \"BOT: Added labels to the crash report\" }";

    let key = findAppName body

    let label = "[ \"" + Map.find key apps + "\" ]";

    if json |> prop ["action"] "none" = "opened" then
        if hasProp ["issue"] json then
            log.Info(
                sprintf "%s posted an issue #%d: %s"
                    (prop ["issue"; "user"; "login"] "Unknown user" json)
                    (prop ["issue"; "number"] -1337 json)
                    (prop ["issue"; "title"] "Unknown title" json)
                    )

            SendGitHubRequest (prop ["issue"; "comments_url"] "" json) comment |> ignore
            SendGitHubRequest (sprintf "%s/labels" (prop ["issue"; "url"] "" json)) label |> ignore
