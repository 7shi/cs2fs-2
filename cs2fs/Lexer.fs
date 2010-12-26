namespace CSharpParser

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Text

type Lexer =
    [<DefaultValue>] static val mutable private Operators : string[]
    [<DefaultValue>] static val mutable private OpDic : Dictionary<char,string[]>
    [<DefaultValue>] static val mutable private OpHeads : string
    [<DefaultValue>] val mutable private clm : int
    [<DefaultValue>] val mutable private lin : int
    [<DefaultValue>] val mutable private pos : int
    [<DefaultValue>] val mutable private cur : char
    [<DefaultValue>] val mutable private src : string

    [<DefaultValue>] val mutable private _Line : int
    member this.Line = this._Line
    member private this.Line with set(value) = this._Line <- value

    [<DefaultValue>] val mutable private _Column : int
    member this.Column = this._Column
    member private this.Column with set(value) = this._Column <- value

    [<DefaultValue>] val mutable private _Position : int
    member this.Position = this._Position
    member private this.Position with set(value) = this._Position <- value

    [<DefaultValue>] val mutable private _Token : string
    member this.Token = this._Token
    member private this.Token with set(value) = this._Token <- value

    [<DefaultValue>] val mutable private _Type : TokenType
    member this.Type = this._Type
    member private this.Type with set(value) = this._Type <- value

    new(src : string) as this = {} then
        if Lexer.Operators = null then
            Lexer.Operators <- [| "."; "("; ")"; "["; "]"; "++"; "--"; "->"; "+"; "-"; "!"; "~"; "&"; "*"; "/"; "%"; "<<"; ">>"; "<"; ">"; "<="; ">="; "=="; "!="; "^"; "|"; "&&"; "||"; "??"; "?:"; "="; "+="; "-="; "*="; "/="; "%="; "&="; "|="; "^="; "<<="; ">>="; "=>"; "?"; ":" |]
            let mutable dic = new Dictionary<char, List<string>>()
            for op in Lexer.Operators do
                if dic.ContainsKey(op.[0]) then
                    dic.[op.[0]].Add(op)
                else
                    let mutable list = new List<string>()
                    dic.[op.[0]] <- list
                    list.Add(op)
            let mutable keys = new List<char>(dic.Keys)
            keys.Sort()
            Lexer.OpHeads <- new String(keys.ToArray())
            Lexer.OpDic <- new Dictionary<char, string[]>()
            for i in Enumerable.Range(0, keys.Count) do
                let mutable key = keys.[i]
                let mutable list = dic.[key]
                list.Sort((fun (a : string) (b : string) ->
                    if a.Length = b.Length then
                        a.CompareTo(b)
                    else
                        b.Length - a.Length
                ))
                Lexer.OpDic.[key] <- list.ToArray()
        this.src <- src
        this.clm <- 1
        this.lin <- 1
        if src.Length > 0 then
            this.cur <- src.[0]

    member private this.MoveNext() =
        if this.pos < this.src.Length then
            this.pos <- this.pos + 1
            this.clm <- this.clm + 1
            if this.pos < this.src.Length then
                this.cur <- this.src.[this.pos]
            else
                this.cur <- (char)0
        else
            this.cur <- (char)0

    member private this.SetResult(t : TokenType) =
        this.Token <- this.src.Substring(this.Position, this.pos - this.Position)
        this.Type <- t

    member this.ReadAllTokens() : Token[] =
        let mutable list = new List<Token>()
        while this.Read() do
            list.Add(new Token(this.Token, this.Type, this.Line, this.Column))
        list.ToArray()

    member this.Read() : bool =
        this.Column <- this.clm
        this.Line <- this.lin
        this.Position <- this.pos
        if this.src = null || this.pos >= this.src.Length then
            this.Token <- ""
            this.Type <- TokenType.None
            false
        else
            if this.cur = ' ' || this.cur = '\t' then
                while Lexer.IsSpace(this.cur) do
                    this.MoveNext()
                this.SetResult(TokenType.Space)
            elif this.cur = '\r' then
                this.MoveNext()
                if this.cur = '\n' then
                    this.MoveNext()
                this.clm <- 1
                this.lin <- this.lin + 1
                this.SetResult(TokenType.NewLine)
            elif this.cur = '\n' then
                this.MoveNext()
                this.clm <- 1
                this.lin <- this.lin + 1
                this.Token <- "\n"
                this.Type <- TokenType.NewLine
            elif this.cur = ';' then
                this.MoveNext()
                this.Token <- ";"
                this.Type <- TokenType.Separator
            elif this.cur = '\'' then
                this.ReadChar()
            elif this.cur = '"' then
                this.ReadString()
            elif this.cur = '{' then
                this.MoveNext()
                this.Token <- "{"
                this.Type <- TokenType.BeginBlock
            elif this.cur = '}' then
                this.MoveNext()
                this.Token <- "}"
                this.Type <- TokenType.EndBlock
            elif this.cur = ',' then
                this.MoveNext()
                this.Token <- ","
                this.Type <- TokenType.Comma
            elif this.cur = '/' && this.IsBeginComment() then
                this.ReadComment()
            elif Char.IsNumber(this.cur) then
                this.ReadNumber()
            elif Lexer.IsFirstLetter(this.cur) then
                while Lexer.IsLetter(this.cur) do
                    this.MoveNext()
                this.Token <- this.src.Substring(this.Position, this.pos - this.Position)
                this.Type <- TokenType.Any
            else
                let mutable op = this.GetOperator()
                if op <> "" then
                    this.pos <- this.pos + op.Length
                    this.clm <- this.clm + op.Length
                    if this.pos < this.src.Length then
                        this.cur <- this.src.[this.pos]
                    else
                        this.cur <- (char)0
                    this.Token <- op
                    this.Type <- TokenType.Operator
                else
                    this.MoveNext()
                    this.SetResult(TokenType.None)
                    raise <| this.Abort("invalid character")
            true

    static member IsSpace(ch : char) : bool =
        ch = ' ' || ch = '\t'

    static member IsFirstLetter(ch : char) : bool =
        ch = '_' || ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z') || ch >= (char)128

    static member IsLetter(ch : char) : bool =
        Lexer.IsFirstLetter(ch) || Char.IsNumber(ch)

    static member IsNewLine(ch : char) : bool =
        ch = '\r' || ch = '\n'

    member private this.IsBeginComment() : bool =
        if this.pos + 1 < this.src.Length then
            let mutable ch = this.src.[this.pos + 1]
            this.cur = '/' && (ch = '/' || ch = '*')
        else
            false

    member private this.IsEndComment() : bool =
        if this.pos + 1 < this.src.Length then
            this.cur = '*' && this.src.[this.pos + 1] = '/'
        else
            false

    member private this.ReadComment() =
        this.MoveNext()
        if this.cur = '/' then
            while not (Lexer.IsNewLine(this.cur)) do
                this.MoveNext()
            this.SetResult(TokenType.Comment1)
        else
            while not (this.IsEndComment()) do
                this.MoveNext()
            if this.IsEndComment() then
                this.MoveNext()
                this.MoveNext()
                this.SetResult(TokenType.Comment)
            else
                raise <| this.Abort("unterminated comment")

    member private this.ReadString() =
        this.MoveNext()
        while this.cur <> '"' do
            if this.cur = '\\' then
                this.MoveNext()
            this.MoveNext()
        if this.cur = '"' then
            this.MoveNext()
            this.SetResult(TokenType.String)
        else
            raise <| this.Abort("unterminated string")

    member private this.ReadChar() =
        this.MoveNext()
        if this.cur = '\\' then
            this.MoveNext()
        this.MoveNext()
        if this.cur = '\'' then
            this.MoveNext()
            this.SetResult(TokenType.Char)
        else
            raise <| this.Abort("unterminated character")

    member private this.GetOperator() : string =
        if not (Lexer.OpHeads.Contains(this.cur)) then
            ""
        else
            let mutable max = this.src.Length - this.pos
            let mutable ret = ""
            for op in Lexer.OpDic.[this.cur] do
                if ret = "" && op.Length <= max && this.src.Substring(this.pos, op.Length) = op then
                    ret <- op
            ret

    member private this.ReadNumber() =
        while Char.IsNumber(this.cur) do
            this.MoveNext()
        if this.cur = '.' then
            this.ReadFloat()
        else
            let mutable ch2 = Char.ToLower(this.cur)
            if ch2 = 'u' then
                this.MoveNext()
                if Char.ToLower(this.cur) = 'l' then
                    this.MoveNext()
                    this.SetResult(TokenType.ULong)
                else
                    this.SetResult(TokenType.UInt)
            elif ch2 = 'l' then
                this.MoveNext()
                this.SetResult(TokenType.Long)
            else
                this.SetResult(TokenType.Int)

    member private this.ReadFloat() =
        while Char.IsNumber(this.cur) do
            this.MoveNext()
        let mutable ch2 = Char.ToLower(this.cur)
        if ch2 = 'f' then
            this.MoveNext()
            this.SetResult(TokenType.Float)
        elif ch2 = 'd' then
            this.MoveNext()
            this.SetResult(TokenType.Double)
        else
            this.SetResult(TokenType.Double)

    member private this.Abort(message : string) : Exception =
        new Exception(String.Format("[{0},{1}] {2}: {3}", this.Line, this.Column, message, this.Token))
