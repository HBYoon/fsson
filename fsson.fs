// F# JSON
// The MIT License
// Copyright (c) 2013 HBYoon
// (yy/mm/dd)
// 13/09/17 - 0.0.1 fsson start

module fsson

open System
open System.Collections.Generic

let ( <<= ) = ( := )

let ( += ) (n:ref<int>) k = 
    n := !n + k

let (<<<!) a b =
    a (ref 0) b ()

exception JsonParseError of string
exception JsonTypeError  of string

// JSON Data type
type JsonType =
    | String = 0
    | Number = 1
    | Object = 2
    | Array  = 3
    | Bool   = 4
    | Null   = 5

// JSON record type
type JSON = {
    Type: JsonType
    Str:  string option // escaped string
    Num:  double option
    Obj:  Dictionary<string,JSON> option
    Arr:  ResizeArray<JSON> option // gen list
    Boo:  bool option
}



module BaseUtil = 
    let rec FirstCharAndRealLength (str:string) count =
        if str.Length = count then (' ', str.Length) else
        match str.[count] with
        | ' ' | '\t' | '\n' -> FirstCharAndRealLength str (count+1)
        | _ as c -> (c, str.Length - count)

    let rec StrMatch (str:string) strPointList charList =
        match (strPointList, charList) with
        | (hPoint::tPoint, hChar::tChar) 
            when str.[hPoint] = hChar -> StrMatch str tPoint tChar
        | ([],[]) -> true
        | _ -> false

module JNull =
    let Null = {
        Type= JsonType.Null
        Str = None
        Num = None
        Obj = None
        Arr = None
        Boo = None
    }
    module Parse =
        let Ready refC (str:string) () =
            let sc = !refC
            match str.[sc] with
            | 'n' | 'N' ->  
                match BaseUtil.StrMatch str [(sc+1) .. (sc+3)] ['u'; 'l'; 'l'] with
                | true  -> refC += 4; Null
                | false -> raise (JsonParseError "Null parse error")
            | _ -> raise (JsonParseError "Null parse error")

        let Run str = Ready <<<! str

module JBool =
    let WrapJson boo =
        {
            Type= JsonType.Bool
            Str = None
            Num = None
            Obj = None
            Arr = None
            Boo = Some(boo)
        }

    let Value (js:JSON) =
        match js.Type with
        | JsonType.Bool -> js.Boo.Value
        | _ -> raise (JsonTypeError ("JsonBool value type error, argument type:" + js.Type.ToString()))

    module Parse =
        let Ready refC (str:string) () =
            let sc = !refC
            match str.[sc] with
            | 't' | 'T' ->
                match BaseUtil.StrMatch str [(sc+1) .. (sc+3)] ['r'; 'u'; 'e'] with
                | true  -> refC += 4; WrapJson(true)
                | false -> raise (JsonParseError "Bool parse error")
            | 'f' | 'F' ->
                match BaseUtil.StrMatch str [(sc+1) .. (sc+4)] ['a'; 'l'; 's'; 'e'] with
                | true  -> refC += 5; WrapJson(false)
                | false -> raise (JsonParseError "Bool parse error")
            | _ -> raise (JsonParseError "Bool parse error")

        let Run str = Ready <<<! str


module JNumber =
    let (| Number | _ |) num = 
        match num with
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> Some(Number)
        | _ -> None

    let Value (js:JSON) =
        match js.Type with
        | JsonType.Number -> js.Num.Value
        | _ -> raise (JsonTypeError ("JsonNumber value type error, argument type:" + js.Type.ToString()))

    let WrapJson num = 
        {
            Type= JsonType.Number
            Str = None
            Num = Some(num)
            Obj = None
            Arr = None
            Boo = None
        }
    module Parse =

        let Ready refC (str:string) () =
            let sc = !refC
            let length = str.Length
            let rec numSet count =
                match str.[count] with
                | '-' | Number -> lengthCheck numCount (count+1)
                | _ -> raise (JsonParseError "Number parse error")
            and numCount count =
                match str.[count] with
                | Number -> lengthCheck numCount (count+1)
                | '.' -> lengthCheck realNum (count+1)
                | 'e' | 'E' -> lengthCheck nextOfE (count+1)
                | _ -> count
            and realNum count =
                match str.[count] with
                | Number -> lengthCheck realNum (count+1)
                | 'e' | 'E' -> lengthCheck nextOfE (count+1)
                | _ -> count
            and nextOfE count = 
                match str.[count] with
                | Number | '-' | '+' -> lengthCheck numberLoop (count+1)
                | _ -> raise (JsonParseError "Number parse error")
            and numberLoop count =
                match str.[count] with
                | Number -> lengthCheck numberLoop (count+1)
                | _ -> count
            and lengthCheck matchFun count =
                match count = length with
                | true  -> count
                | false -> matchFun count
            let numberLength = (lengthCheck numSet sc) - sc

            refC += numberLength

            (sc, numberLength) 
            |> str.Substring 
            |> double 
            |> WrapJson 


        let Run str = Ready <<<! str

module JString =
    let Value (js:JSON) =
        match js.Type with
        | JsonType.String -> js.Str.Value
        | _ -> raise (JsonTypeError ("JsonString value type error, argument type:" + js.Type.ToString()))

    let WrapJson str =
        {
            Type= JsonType.String
            Str = Some(str)
            Num = None
            Obj = None
            Arr = None
            Boo = None
        }

    let Unescape (str:string) = 
        let refJ = ref 0
        new string( 
            [| for i = 0 to (str.Length-1) do
                let c = str.[i]
                match !refJ with
                | 0 -> 
                  yield 
                    match c with
                    | '\u005c' ->
                        refJ <<= 1
                        let nc = str.[i+1]
                        match nc with
                        | '\u005c' -> '\u005c'
                        | '"' -> '\u0022'
                        | '/' -> '/'
                        | 'b' -> '\u0008'
                        | 'f' -> '\u000c'
                        | 'n' -> '\u000a'
                        | 'r' -> '\u000d'
                        | 't' -> '\u0009'
                        | 'u' -> 
                            try 
                                refJ <<= 5 
                                char(int("0x"+str.Substring(i+2,4)))
                            with
                            | :? System.ArgumentOutOfRangeException 
                            | :? System.FormatException -> 
                                refJ <<= 1
                                'u'
                        | _ as nc -> nc
                    | _ -> c 
                | _ when !refJ > 0 -> refJ += (-1)
                | _ -> refJ <<= 0
            |])

    let Escape (str:string)= 
        new string(
            [| for i = 0 to (str.Length - 1) do
                let c = str.[i]
                if c < '\u0020' then
                    match c with
                    | '\u0022' -> 
                        yield '\\'
                        yield '"'
                    | '\u005c' -> 
                        yield '\\'
                        yield '\\'
                    | '\u002f' ->
                        yield '\\'
                        yield '/'
                    | '\u0008' ->
                        yield '\\'
                        yield 'b'
                    | '\u000c' ->
                        yield '\\'
                        yield 'f'
                    | '\u000a' ->
                        yield '\\'
                        yield 'n'
                    | '\u000d' ->
                        yield '\\'
                        yield 'r'
                    | '\u0009' ->
                        yield '\\'
                        yield 't'
                    | _ -> 
                        let hexC = (byte(c)).ToString("X2")
                        yield '\\'
                        yield 'u'
                        yield '0'
                        yield '0'
                        yield hexC.[0]
                        yield hexC.[1]
                else 
                    match c with
                    | '\u0022' -> 
                        yield '\\'
                        yield '"'
                    | '\u005c' -> 
                        yield '\\'
                        yield '\\'
                    | '\u002f' ->
                        yield '\\'
                        yield '/'
                    | _ -> yield c 
            |])

    module Parse =

        let CheckStr (str:string) count =
            let length = str.Length
            let rec checkLoop needEscape countNow =
                if countNow = length then
                    (needEscape, (countNow - count))
                else 
                    if str.[countNow] < char(' ') then
                        checkLoop true (countNow+1)
                    else
                        match str.[countNow] with
                        | '\u005c' -> checkLoop needEscape (countNow+2)
                        | '"'      -> (needEscape, (countNow - count))
                        |  _       -> checkLoop needEscape (countNow+1)
            checkLoop false count

        let GetStr refC (str:string) quot =
            let strStart = !refC
            let (needEscape, length) = CheckStr str strStart
            refC += if quot then (length+1) else length
            match needEscape with
            | true  -> Escape(str.Substring(strStart, length))
            | false -> str.Substring(strStart, length)

        let Ready refC (str:string) () =
            match str.[!refC] with
            | '"' -> refC += 1; WrapJson(GetStr refC str true)
            | _   -> WrapJson(GetStr refC str false)

        let Run str = Ready <<<! str


module JObject =
    let rec Get (json:JSON) getList =
        match getList with
        | [] -> json
        | h::tList ->
        match json.Type with
        | JsonType.Array -> 
            let refK = ref 0
            match System.Int32.TryParse(h, refK) with
            | true  -> Get (json.Arr.Value.Item(!refK)) tList
            | false -> JNull.Null
        | JsonType.Object ->
            Get (json.Obj.Value.Item(h)) tList
        | _ -> JNull.Null

    let Set (js:JSON) key vaule =
        match js.Type with
        | JsonType.Object -> js.Obj.Value.Add(key, vaule)
        | _ -> raise (JsonTypeError ("JsonObject Set type error, argument type:" + js.Type.ToString()))

    let Del (js:JSON) key =
        match js.Type with
        | JsonType.Object -> js.Obj.Value.Remove key |> ignore
        | _ -> raise (JsonTypeError ("JsonObject Del type error, argument type:" + js.Type.ToString()))

    let WrapJson obj =
        {
            Type= JsonType.Object
            Str = None
            Num = None
            Obj = Some(obj)
            Arr = None
            Boo = None
        }

    let Create () =
        WrapJson (new Dictionary<string,JSON>())
        

    type ObjSet (js:JSON) =
        member this.Set = Set js
        member this.Del = Del js

module JArray =
    let Get = JObject.Get

    let Push (js:JSON) value = 
        match js.Type with
        | JsonType.Array -> js.Arr.Value.Add(value)
        | _ -> raise (JsonTypeError ("JsonArray Push type error, argument type:" + js.Type.ToString()))

    let Unshift (js:JSON) value =
        match js.Type with
        | JsonType.Array ->
            js.Arr.Value.Reverse()
            js.Arr.Value.Add(value)
            js.Arr.Value.Reverse()
        | _ -> 
            raise (JsonTypeError ("JsonObject Unshift type error, argument type:" + js.Type.ToString()))

    let Pop (js:JSON) =
        match js.Type with
        | JsonType.Array ->
            let lastKey = js.Arr.Value.Count - 1
            let rJson = js.Arr.Value.Item(lastKey)
            js.Arr.Value.RemoveRange(lastKey, 1)
            rJson
        | _ -> 
            raise (JsonTypeError ("JsonObject Pop type error, argument type:" + js.Type.ToString()))

    let Shift (js:JSON) =
        match js.Type with
        | JsonType.Array ->
            let rJson = js.Arr.Value.Item(0)
            js.Arr.Value.RemoveRange(0, 1)
            rJson
        | _ -> 
            raise (JsonTypeError ("JsonObject Shift type error, argument type:" + js.Type.ToString()))

    let WrapJson arr =
        {
            Type= JsonType.Array
            Str = None
            Num = None
            Obj = None
            Arr = Some(arr)
            Boo = None
        }

    let Create () =
        WrapJson (new ResizeArray<JSON>())

    type ArrSet (js:JSON) =
        member this.Pop = fun() -> Pop js
        member this.Push = Push js
        member this.Shift = fun () -> Shift js
        member this.Unshift = Unshift js
        
        
module Parse =
    let Run str =
        let refC = ref 0

        let nulP = JNull.Parse.Ready refC str
        let booP = JBool.Parse.Ready refC str
        let numP = JNumber.Parse.Ready refC str
        let strP = JString.Parse.Ready refC str

        let rec parseLoop () =
            match str.[!refC] with
            | '"' -> strP()
            | 'n' -> nulP()
            | 't' | 'f' -> booP()
            | '-' | JNumber.Number -> numP()
            | ' ' | '\t' | '\n' | ',' -> refC+=1; parseLoop()
            | '{' -> refC+=1; Obj()
            | '[' -> refC+=1; Arr()
            | _ -> raise (JsonParseError "JSON parse error")
        and Obj () =
            let dictionary = new Dictionary<string, JSON>()
            let rec set () =
                match str.[!refC] with
                | '"' ->
                    refC += 1
                    let key = JString.Parse.GetStr refC str true
                    let value = checkColon()
                    dictionary.Add (key, value)
                    set()
                | '}' -> refC+=1; dictionary
                |  ' ' | ','  -> refC += 1; set()
                | _ -> raise (JsonParseError  "Object parse error, undefined vaule")
            JObject.WrapJson(set())
        and checkColon () =
            match str.[!refC] with
            | ' ' -> refC += 1; checkColon()
            | ':' -> refC += 1; parseLoop()
            | _   -> raise (JsonParseError  "Object parse error, no colon between key/value")

        and Arr () =
            let list = new ResizeArray<JSON>()
            let rec set () =
                match str.[!refC] with
                | ']' -> refC+=1; list
                | ' ' | ',' -> refC+=1; set()
                | _ -> 
                    list.Add(parseLoop())
                    set()
            JArray.WrapJson(set())

        match BaseUtil.FirstCharAndRealLength str 0 with
        | (' ', _) -> JNull.Null
        | ('t', 4) | ('f', 5) | ('n', 4) | ('{', _) | ('[', _) -> parseLoop()
        | _ -> strP()

    let AgentCallback (msgBox:MailboxProcessor<AsyncReplyChannel<JSON> * string>) = 
        async {
            let! (rc, str) = msgBox.Receive()
            rc.Reply(Run str)
        }

    let AsyncRun str =
        async {
            return Run str
        }
        
module Stringify =
    let rec Run (js:JSON) =
        match js.Type with
        | JsonType.String -> js.Str.Value
        | JsonType.Number -> js.Num.Value.ToString()
        | JsonType.Bool -> 
            if js.Boo.Value = true then 
                "true" 
            else 
                "false"
        | JsonType.Object -> ObjToStr(js)
        | JsonType.Array ->  ArrToStr(js)
        | JsonType.Null | _ -> "null"
    and ObjToStr (js:JSON) =
        "{" + String.Join(",",
            [| for i in js.Obj.Value do
                match i.Value.Type with
                | JsonType.String -> yield "\"" + i.Key + "\":\"" + i.Value.Str.Value + "\""
                | _               -> yield "\"" + i.Key + "\":" + Run(i.Value) |]) + "}"
    and ArrToStr (js:JSON) =
        "[" + String.Join(",",
            [| for i in js.Arr.Value do
                match i.Type with
                | JsonType.String -> yield "\"" + i.Str.Value + "\""
                | _               -> yield Run (i) |]) + "]"

    let AgentCallback (msgBox:MailboxProcessor<AsyncReplyChannel<string> * JSON>) = 
        async {
            let! (rc, js) = msgBox.Receive()
            rc.Reply(Run js)
        }

    let AsyncRun js =
        async {
            return Run js
        }

type JSON with
    member this.ToString = fun () -> Stringify.Run this
    member this.Get = JObject.Get this
    member this.Object = JObject.ObjSet this
    member this.Array = JArray.ArrSet this