namespace CSharpParser

open System
open System.IO

type Debug() =
    [<DefaultValue>] static val mutable private stream : TextWriter
    static member Stream = Debug.stream
    static member Stream with set(value) = Debug.stream <- value

    static member Write(t:string) = Debug.stream.Write(t)
    static member Write(t:string, arg1:obj) = Debug.stream.Write(t, arg1)
    static member Write(t:string, arg1:obj, arg2:obj) = Debug.stream.Write(t, arg1, arg2)
    static member WriteLine() = Debug.stream.WriteLine()
    static member WriteLine(t:string) = Debug.stream.WriteLine(t)
    static member WriteLine(t:string, arg1:obj) = Debug.stream.WriteLine(t, arg1)
    static member WriteLine(t:string, arg1:obj, arg2:obj) = Debug.stream.WriteLine(t, arg1, arg2)
