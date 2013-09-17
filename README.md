fsson
===
F# library for JSON string data.

JSON Record Type
---
```
type JsonType =
    | String = 0
    | Number = 1
    | Object = 2
    | Array  = 3
    | Bool   = 4
    | Null   = 5

type JSON = {
    Type: JsonType
    Str:  string option
    Num:  double option
    Obj:  Dictionary<string,JSON> option
    Arr:  ResizeArray<JSON> option
    Boo:  bool option
}
```

Base Structure
---
###Value Type module
---
Each module have a typed string parser.
```
JNull
JBool
JString
JNumber
```
###Object Type module
---
Each module have a data handling method for own type.
```
JObject
JArray
```
###Tool module
---
```
Parse
Stringify
```
###JSON member
---
```
ToString
Get
Object
Array
```
How to use
---
```
// create JSON record from JSON string
let js = fsson.Parse.Run("[1,2,3,\"test\",{\"test\":true}]")

// take JSON record in Object type JSON record
let testStr = js.Get ["3"]
let innerObj = js.Get ["4"]
let testInObj = js.Get ["4"; "test"]

// array method
js.Array.Push (fsson.Parse.Run("1234"))
js.Array.Unshift (fsson.Parse.Run("test string"))
let num1234 = js.Array.Pop()
let testString = js.Array.Shift()

// create new Object Type JSON record
let obj = fsson.JObject.Create()

// insert value
obj.Object.Set "str"  (fsson.JString.Parse.Run("test string"))
obj.Object.Set "bool" (fsson.JBool.WrapJson(true))
obj.Object.Set "num"  (fsson.JNumber.WrapJson(123.0))
obj.Object.Set "null" (fsson.JNull.Null)

// delete value
obj.Object.Del "num"

// stringify
let objStr = obj.ToString()
```

