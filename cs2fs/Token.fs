namespace CSharpParser

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Text

type TokenType =
    | None = 0
    | Space = 1
    | NewLine = 2
    | Any = 3
    | Operator = 4
    | Separator = 5
    | Int = 6
    | UInt = 7
    | Long = 8
    | ULong = 9
    | Float = 10
    | Double = 11
    | String = 12
    | Char = 13
    | Comment = 14
    | Comment1 = 15
    | BeginBlock = 16
    | EndBlock = 17
    | Comma = 18

type Token =

    [<DefaultValue>] val mutable private _Text : string
    member this.Text = this._Text
    member private this.Text with set(value) = this._Text <- value

    [<DefaultValue>] val mutable private _Type : TokenType
    member this.Type = this._Type
    member private this.Type with set(value) = this._Type <- value

    [<DefaultValue>] val mutable private _Line : int
    member this.Line = this._Line
    member private this.Line with set(value) = this._Line <- value

    [<DefaultValue>] val mutable private _Column : int
    member this.Column = this._Column
    member private this.Column with set(value) = this._Column <- value

    new(name : string, t : TokenType, line : int, column : int) as this = {} then
        this.Text <- name
        this.Type <- t
        this.Line <- line
        this.Column <- column

    member this.Align(tab : int) : string =
        if this.Type <> TokenType.Space then
            this.Text
        else
            let mutable sw = new StringWriter()
            let mutable column = this.Column
            for ch in this.Text do
                if ch = '\t' then
                    let mutable len = tab - ((column - 1) % tab)
                    sw.Write(new String(' ', len))
                    column <- column + len
                else
                    sw.Write(ch)
                    column <- column + 1
            sw.Close()
            sw.ToString()

    member this.CanOmit = this.Type = TokenType.Space || this.Type = TokenType.NewLine || this.Type = TokenType.Comment || this.Type = TokenType.Comment1

    member this.Write(tw : TextWriter) =
        tw.Write("[{0}, {1}] {2}: ", this.Line, this.Column, this.Type)
        match this.Type with
        | TokenType.Space ->
            tw.WriteLine("{0}", this.Align(4).Length)
        | TokenType.NewLine ->
            tw.WriteLine(this.Text.Replace("\r", "\\r").Replace("\n", "\\n"))
        | _ ->
            tw.WriteLine(this.Text)
