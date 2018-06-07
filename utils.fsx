type String = System.String
open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Net.Http
open Newtonsoft.Json


/// Returns if the string is null or empty
let inline isNullOrEmpty value = String.IsNullOrEmpty value

/// Returns if the string is not null or empty
let inline isNotNullOrEmpty value = String.IsNullOrEmpty value |> not

/// Returns if the string is null or empty or completely whitespace
let inline isNullOrWhiteSpace value = isNullOrEmpty value || value |> Seq.forall Char.IsWhiteSpace

/// Replaces the given pattern in the given text with the replacement
let inline replace (pattern : string) replacement (text : string) = text.Replace(pattern, replacement)

/// Converts a sequence of strings to a string with delimiters
let inline separated delimiter (items : string seq) = String.Join(delimiter, Array.ofSeq items)

/// Removes the slashes from the end of the given string
let inline trimSlash (s : string) = s.TrimEnd('\\')

/// Splits the given string at the given char delimiter
let inline split (delimiter : char) (text : string) = text.Split [| delimiter |] |> Array.toList

/// Splits the given string at the given string delimiter
let inline splitStr (delimiterStr : string) (text : string) = 
    text.Split([| delimiterStr |], StringSplitOptions.None) |> Array.toList

/// Converts a sequence of strings into a string separated with line ends
let inline toLines text = separated Environment.NewLine text

/// Checks whether the given text starts with the given prefix
let startsWith prefix (text : string) = text.StartsWith prefix

/// Checks whether the given text ends with the given suffix
let endsWith suffix (text : string) = text.EndsWith suffix

/// Determines whether the last character of the given <see cref="string" />
/// matches Path.DirectorySeparatorChar.         
let endsWithSlash = endsWith (Path.DirectorySeparatorChar.ToString())

/// Replaces the first occurrence of the pattern with the given replacement.
let replaceFirst (pattern : string) replacement (text : string) = 
    let pos = text.IndexOf pattern
    if pos < 0 then text
    else text.Remove(pos, pattern.Length).Insert(pos, replacement)

/// Regex stuff
let private regexes = new Dictionary<_, _>()
let getRegEx pattern = 
    match regexes.TryGetValue pattern with
    | true, regex -> regex
    | _ -> (new System.Text.RegularExpressions.Regex(pattern))

let regex_replace pattern (replacement : string) text = (getRegEx pattern).Replace(text, replacement)

/// Checks whether the given char is a german umlaut.
let isUmlaut c = Seq.exists ((=) c) [ 'ä'; 'ö'; 'ü'; 'Ä'; 'Ö'; 'Ü'; 'ß' ]

/// Converts all characters in a string to lower case.
let inline toLower (s : string) = s.ToLower()

/// Returns all standard chars and digits.
let charsAndDigits = [ 'a'..'z' ] @ [ 'A'..'Z' ] @ [ '0'..'9' ]

/// Checks whether the given char is a standard char or digit.
let isLetterOrDigit c = List.exists ((=) c) charsAndDigits

/// Trims the given string with the DirectorySeparatorChar
let inline trimSeparator (s : string) = s.TrimEnd Path.DirectorySeparatorChar

/// Trims all special characters from a string.
let inline trimSpecialChars (text : string) = 
    text
    |> Seq.filter isLetterOrDigit
    |> Seq.filter (isUmlaut >> not)
    |> Seq.fold (fun (acc : string) c -> acc + string c) ""

/// Trims the given string
let inline trim (x : string) = 
    if isNullOrEmpty x then x
    else x.Trim()

/// Trims the given string
let inline trimChars chars (x : string) = 
    if isNullOrEmpty x then x
    else x.Trim chars

/// Trims the start of the given string
let inline trimStartChars chars (x : string) =
    if isNullOrEmpty x then x
    else x.TrimStart chars

/// Trims the end of the given string
let inline trimEndChars chars (x : string) =
    if isNullOrEmpty x then x
    else x.TrimEnd chars

/// Lifts a string to an option
let liftString x = 
    if isNullOrEmpty x then None
    else Some x


/// Removes all trailing .0 from a version string
let rec NormalizeVersion(version : string) =
    if version = null then "" else
    let elements = version.Split [| '.' |]
    let mutable version = ""
    for i in 0..3 do
        if i < elements.Length then 
            if version = "" then version <- elements.[i]
            else version <- version + "." + elements.[i]
    if version.EndsWith ".0" then version.Remove(version.Length - 2, 2) |> NormalizeVersion
    else version

let Colon = ','


/// Removes linebreaks from the given string
let inline removeLineBreaks text = 
    text
    |> replace "\r" String.Empty
    |> replace "\n" String.Empty

/// Encapsulates the Apostrophe
let inline encapsulateApostrophe text = replace "'" "`" text

/// Decodes a Base64-encoded UTF-8-encoded string
let decodeBase64Utf8String(text : string) = 
    text
    |> Convert.FromBase64String
    |> Encoding.UTF8.GetString

/// Operator overloads 

/// Checks whether the given text starts with the given prefix
let inline (<*) prefix text = startsWith prefix text

/// Find a regex pattern in a text and replaces it with the given replacement.
let (>=>) pattern replacement text = regex_replace pattern replacement text

/// Determines if a text matches a given regex pattern.
let (>**) pattern text = (getRegEx pattern).IsMatch text

module Slack = 
    type NotificationAttachmentFieldParams = {
        Title: string
        Value: string
        Short: bool
    }
    
    type NotificationAttachmentParams = {
        Fallback: string
        Title: string
        TitleLink: string
        Text: string
        Pretext: string
        Color: string
        Fields: NotificationAttachmentFieldParams[]
    }
    
    type NotificationParams = {
        Text: string
        From: string
        Channel: string
        IconURL: string
        IconEmoji: string
        UnfurlLinks: bool
        Attachments: NotificationAttachmentParams[]
        LinkNames: bool
    }
    let NotificationDefaults = {
        Text = ""
        From = null
        Channel = null
        IconURL = null
        IconEmoji = null
        UnfurlLinks = false
        Attachments = Array.empty
        LinkNames = false
    }
    
    let NotificationAttachmentDefaults = {
        Fallback = ""
        Title = null
        TitleLink = null
        Text = null
        Pretext = null
        Color = null
        Fields = Array.empty
    }
    
    let NotificationAttachmentFieldDefaults = {
        Title = ""
        Value = ""
        Short = false
    }
    
    let private lowerCaseContractResolver = { new Newtonsoft.Json.Serialization.DefaultContractResolver() with
        override this.ResolvePropertyName (key : string) =
            key.ToLower()
    }
    
    let private ValidateParams webhookURL (param : NotificationParams) =
        if webhookURL = "" then failwith "You must specify a webhook URL"
        if param.Text = "" && param.Attachments.Length = 0 then failwith "You must specify a message or include an attachment"
        let validateField (field : NotificationAttachmentFieldParams) =
            if field.Title = "" then failwith "Each field must have a title"
            if field.Value = "" then failwith "Each field must have a value"
        let validateAttachment (attachment : NotificationAttachmentParams) =
            if attachment.Fallback = "" then failwith "Each attachment must have a fallback"
            Array.iter(fun field -> validateField field) attachment.Fields
        Array.iter(fun attachment -> validateAttachment attachment) param.Attachments
    
        param
    
    let private SerializeData data =
        JsonConvert.SerializeObject(data, Formatting.None, new JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore, ContractResolver = lowerCaseContractResolver))
    let sendNotification (webhookURL : string) (setParams: NotificationParams -> NotificationParams) =
        let sendNotification param =
            use client = (new HttpClient())
            let response = client.PostAsync(webhookURL, new StringContent(SerializeData param, System.Text.Encoding.UTF8, "application/json")).Result
            response.Content.ReadAsStringAsync().Result
        NotificationDefaults 
        |> setParams
        |> ValidateParams webhookURL
        |> sendNotification

module ReleaseNotes

open System
open Fake.Core

/// Contains the parsed information of the release notes text file.
type ReleaseNotes =
    { /// The parsed version.
      AssemblyVersion: string
      /// The nuget package version.
      NugetVersion: string
      /// Semantic version
      SemVer: SemVer.SemVerInfo
      /// Release date
      Date : DateTime option
      // The parsed release notes.
      Notes: string list }
    override x.ToString() = sprintf "%A" x

    static member New(assemblyVersion,nugetVersion,date,notes) = { 
        AssemblyVersion = assemblyVersion
        NugetVersion = nugetVersion
        SemVer = SemVer.parse nugetVersion
        Date = date
        Notes = notes }

    static member New(assemblyVersion,nugetVersion,notes) = ReleaseNotes.New(assemblyVersion,nugetVersion,None,notes)

let parseVersions =
    let nugetRegex = String.getRegEx @"([0-9]+.)+[0-9]+(-[a-zA-Z]+\d*)?(.[0-9]+)?"
    let assemblyVersionRegex = String.getRegEx @"([0-9]+.)+[0-9]+"
    fun line ->
        let assemblyVersion = assemblyVersionRegex.Match line
        if not assemblyVersion.Success
        then failwithf "Unable to parse valid Assembly version from release notes (%s)." line

        let nugetVersion = nugetRegex.Match line
        if not nugetVersion.Success
        then failwithf "Unable to parse valid NuGet version from release notes (%s)." line
        assemblyVersion, nugetVersion

let parseDate =
    let dateRegex = String.getRegEx @"(19|20)\d\d([- /.])(0[1-9]|1[012]|[1-9])\2(0[1-9]|[12][0-9]|3[01]|[1-9])"
    fun line ->
        let possibleDate = dateRegex.Match line
        match possibleDate.Success with
        | false -> None
        | true ->
            match DateTime.TryParse possibleDate.Value with
            | false, _ -> None
            | true, x -> Some(x)

/// Parse simple release notes sequence
let private parseSimpleReleaseNotes line =
    let assemblyVersion, nugetVersion = parseVersions line
    let trimDot (s:string) = s.TrimEnd('.')

    let notes = 
        line.Substring (nugetVersion.Index + nugetVersion.Length)
        |> String.trimChars [|' '; '-'|]
        |> String.splitStr ". "
        |> List.map (trimDot >> String.trim)
        |> List.filter String.isNotNullOrEmpty
        |> List.map (fun x -> x + ".")
    ReleaseNotes.New(assemblyVersion.Value, nugetVersion.Value, None, notes)

open Fake.Core.String.Operators

/// Parse "complex" release notes text sequence
let private parseAllComplexReleaseNotes (text: seq<string>) =
    let rec findNextNotesBlock text =
        let isHeader line = "##" <* line
        let rec findEnd notes text =
            match text with
            | [] -> notes,[]
            | h :: rest -> if isHeader h then notes,text else findEnd (h :: notes) rest

        match text with
        | [] -> None
        | h :: rest -> if isHeader h then Some(h,findEnd [] rest) else findNextNotesBlock rest

    let rec loop releaseNotes text =
        match findNextNotesBlock text with
        | Some(header,(notes, rest)) ->
            let assemblyVer, nugetVer = parseVersions header
            let date = parseDate header
            let newReleaseNotes = ReleaseNotes.New(assemblyVer.Value,nugetVer.Value,date,notes |> List.filter String.isNotNullOrEmpty |> List.rev)
            loop (newReleaseNotes::releaseNotes) rest
        | None -> releaseNotes

    loop [] (text |> Seq.map (String.trimStartChars [|' '; '*'|] >> String.trimEndChars [|' '|]) |> Seq.toList)


/// Parses a Release Notes text and returns all release notes.
///
/// ## Parameters
///  - `data` - Release notes text
let parseAllReleaseNotes (data: seq<string>) = 
    let data = data |> Seq.toList |> List.filter (not << String.isNullOrWhiteSpace)
    match data with
    | [] -> failwith "Empty Release file."
    | h :: _ ->
        let (|Simple|Complex|Invalid|) = function '*' -> Simple | '#' -> Complex | _ -> Invalid
        let firstNonEmptyChar = h.Trim([|'-'; ' '|]).[0]
        match firstNonEmptyChar with
        | Simple -> 
            data 
            |> Seq.map parseSimpleReleaseNotes 
            |> Seq.toList
        | Complex -> parseAllComplexReleaseNotes data
        | Invalid -> failwith "Invalid Release Notes format."
        |> List.sortBy (fun x -> x.SemVer)
        |> List.rev

    
/// Parses a Release Notes text and returns the lastest release notes.
///
/// ## Parameters
///  - `data` - Release notes text
let parseReleaseNotes (data: seq<string>) =
    data
    |> parseAllReleaseNotes
    |> Seq.head

/// Parses a Release Notes text file and returns the lastest release notes.
///
/// ## Parameters
///  - `fileName` - Release notes text file name
let LoadReleaseNotes fileName =
    System.IO.File.ReadLines fileName
    |> parseReleaseNotes
