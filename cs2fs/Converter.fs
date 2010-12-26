namespace CSharpParser

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.IO

type TypeName =

    [<DefaultValue>] val mutable private _Type : string
    member this.Type = this._Type
    member private this.Type with set(value) = this._Type <- value

    [<DefaultValue>] val mutable private _Name : string
    member this.Name = this._Name
    member private this.Name with set(value) = this._Name <- value

    new(t : string, name : string) as this = {} then
        this.Type <- t
        this.Name <- name

type Converter =
    [<DefaultValue>] static val mutable private noop : string[]
    [<DefaultValue>] val mutable private tokens : Token[]
    [<DefaultValue>] val mutable private cur : Token
    [<DefaultValue>] val mutable private last : Token
    [<DefaultValue>] val mutable private pos : int
    [<DefaultValue>] val mutable private indent : string
    [<DefaultValue>] val mutable private usings : List<string>
    [<DefaultValue>] val mutable private isNew : bool

    new(tokens : Token[]) as this = {} then
        if Converter.noop = null then
            Converter.noop <- [| "++"; "--"; "??"; "?:"; "+="; "-="; "*="; "/="; "%="; "&="; "|="; "^="; "<<="; ">>="; "=>"; "?"; ":" |]
        this.usings <- new List<string>()
        this.tokens <- tokens.Where((fun (t : Token) ->
            not t.CanOmit
        )).ToArray()
        this.last <- new Token("", TokenType.None, 0, 0)
        if this.tokens.Length > 0 then
            this.cur <- this.tokens.[0]
        else
            this.cur <- this.last

    member private this.MoveNext() =
        if this.pos < this.tokens.Length then
            this.pos <- this.pos + 1
            if this.pos < this.tokens.Length then
                this.cur <- this.tokens.[this.pos]
            else
                this.cur <- this.last
        else
            this.cur <- this.last

    member this.Convert() =
        while this.cur <> this.last do
            match this.cur.Text with
            | "using" ->
                this.usings.Add(this.ReadUsing())
            | "namespace" ->
                this.ReadNamespace()
            | _ ->
                raise <| this.Abort("syntax error")

    member private this.ReadUsing() : string =
        let mutable sw = new StringWriter()
        this.MoveNext()
        while this.cur.Text <> ";" do
            sw.Write(this.cur.Text)
            this.MoveNext()
        this.MoveNext()
        sw.Close()
        sw.ToString()

    member private this.ReadNamespace() =
        Debug.Write("namespace ")
        this.MoveNext()
        while this.cur.Text <> "{" do
            Debug.Write(this.cur.Text)
            this.MoveNext()
        Debug.WriteLine()
        if this.usings.Count > 0 then
            Debug.WriteLine()
            for u in this.usings do
                Debug.WriteLine("open {0}", u)
        this.MoveNext()
        while this.cur <> this.last && this.cur.Text <> "}" do
            this.ReadNamespaceInternal("private")
        this.MoveNext()

    member private this.ReadNamespaceInternal(access : string) =
        if Converter.IsAccess(this.cur.Text) then
            let mutable acc = this.cur.Text
            this.MoveNext()
            this.ReadNamespaceInternal(acc)
        elif this.cur.Text = "class" then
            this.ReadClass(access)
        elif this.cur.Text = "enum" then
            this.ReadEnum(access)
        else
            raise <| this.Abort("not supported")

    member private this.ReadEnum(access : string) =
        this.MoveNext()
        let mutable name = this.cur.Text
        this.MoveNext()
        Debug.WriteLine()
        Debug.Write("type ")
        if access = "private" then
            Debug.Write("private ")
        Debug.WriteLine("{0} =", name)
        if this.cur.Text <> "{" then
            raise <| this.Abort("must be '{'")
        this.MoveNext()
        let mutable v = 0
        while this.cur <> this.last && this.cur.Text <> "}" do
            let mutable id = this.cur.Text
            this.MoveNext()
            if this.cur.Text = "=" then
                this.MoveNext()
                v <- Int32.Parse(this.cur.Text)
                this.MoveNext()
            Debug.WriteLine("    | {0} = {1}", id, v)
            v <- v + 1
            if this.cur.Text = "," then
                this.MoveNext()
        this.MoveNext()

    member private this.ReadClass(access : string) =
        this.MoveNext()
        let mutable name = this.cur.Text
        this.MoveNext()
        Debug.WriteLine()
        Debug.Write("type ")
        if access = "private" then
            Debug.Write("private ")
        Debug.WriteLine("{0} =", name)
        if this.cur.Text = ":" then
            raise <| this.Abort("inherit not supported")
        if this.cur.Text <> "{" then
            raise <| this.Abort("must be '{'")
        this.MoveNext()
        while this.cur <> this.last && this.cur.Text <> "}" do
            this.ReadMember("private", false)
        this.MoveNext()

    member private this.ReadMember(access : string, isStatic : bool) =
        if this.cur.Text = "static" then
            this.MoveNext()
            this.ReadMember(access, true)
        elif Converter.IsAccess(this.cur.Text) then
            let mutable acc = this.cur.Text
            this.MoveNext()
            this.ReadMember(acc, isStatic)
        else
            let mutable tn = this.ReadDecl(false)
            match this.cur.Text with
            | "(" ->
                this.ReadMethod(tn.Name, tn.Type, access, isStatic)
            | ";" ->
                this.ReadField(tn.Name, tn.Type, access, isStatic)
            | "{" ->
                this.ReadProperty(tn.Name, tn.Type, access, isStatic)
            | "=" ->
                raise <| this.Abort("default value not supported")
            | _ ->
                raise <| this.Abort("syntax error")

    member private this.ReadProperty(name : string, t : string, access : string, isStatic : bool) =
        Debug.WriteLine()
        let mutable autoField = false
        this.MoveNext()
        while this.cur.Text <> "}" do
            let mutable acc = access
            if Converter.IsAccess(this.cur.Text) then
                acc <- this.cur.Text
                this.MoveNext()
            if this.cur.Text = "get" || this.cur.Text = "set" then
                let mutable act = this.cur.Text
                this.MoveNext()
                if this.cur.Text = ";" then
                    this.MoveNext()
                    if not autoField then
                        this.MakeField("_" + name, t, "private", isStatic)
                        autoField <- true
                Debug.Write("    ")
                if isStatic then
                    Debug.Write("static ")
                Debug.Write("member ")
                if acc = "private" then
                    Debug.Write("private ")
                if not isStatic then
                    Debug.Write("this.")
                Debug.Write(name)
                if act = "get" then
                    Debug.Write(" =")
                    if autoField then
                        Debug.WriteLine(" this._" + name)
                    else
                        if this.pos + 1 < this.tokens.Length && this.tokens.[this.pos + 1].Text = "return" then
                            this.MoveNext()
                            this.MoveNext()
                            Debug.Write(" ")
                            this.ReadExpr(false)
                            Debug.WriteLine()
                            if this.cur.Text <> "}" then
                                raise <| this.Abort("block not closed")
                            this.MoveNext()
                        else
                            Debug.WriteLine()
                            this.indent <- new string(' ', 8)
                            this.ReadBlock()
                else
                    Debug.Write(" with set(value) =")
                    if autoField then
                        Debug.WriteLine(" this._" + name + " <- value")
                    else
                        Debug.WriteLine()
                        this.indent <- new string(' ', 8)
                        this.ReadBlock()
            else
                raise <| this.Abort("syntax error")
        this.MoveNext()

    member private this.ReadField(name : string, t : string, access : string, isStatic : bool) =
        this.MoveNext()
        this.MakeField(name, t, access, isStatic)

    member private this.MakeField(name : string, t : string, access : string, isStatic : bool) =
        Debug.Write("    [<DefaultValue>] ")
        if isStatic then
            Debug.Write("static ")
        Debug.Write("val mutable ")
        if access = "private" then
            Debug.Write("private ")
        Debug.WriteLine("{0} : {1}", name, t)

    member private this.ReadMethod(name : string, t : string, access : string, isStatic : bool) =
        Debug.WriteLine()
        Debug.Write("    ")
        if isStatic then
            Debug.Write("static ")
        if t = null then
            this.MoveNext()
            if access = "private" then
                Debug.Write("private ")
            Debug.Write("new(")
            this.ReadArgs()
            Debug.Write(") ")
        else
            Debug.Write("member ")
            if access = "private" then
                Debug.Write("private ")
            if not isStatic then
                Debug.Write("this.")
            Debug.Write(name + "(")
            this.MoveNext()
            this.ReadArgs()
            if t = "void" then
                Debug.Write(") =")
            else
                Debug.Write(") : {0} =", t)
        if this.cur.Text <> "{" then
            raise <| this.Abort("block required")
        if this.pos + 1 < this.tokens.Length && this.tokens.[this.pos + 1].Text = "}" then
            this.MoveNext()
            this.MoveNext()
            if t = null then
                Debug.WriteLine("= {0}", "{}")
            else
                Debug.WriteLine(" ()")
        else
            if t = null then
                Debug.WriteLine("as this = {0} then", "{}")
            else
                Debug.WriteLine()
            this.indent <- new string(' ', 8)
            this.ReadBlock()

    member private this.ReadArgs() =
        while this.cur.Text <> ")" do
            this.ReadArg()
            if this.cur.Text = "," then
                Debug.Write(", ")
                this.MoveNext()
        this.MoveNext()

    member private this.ReadDecl(arg : bool) : TypeName =
        let mutable list = new List<string>()
        let mutable seps = "(){};="
        if arg then
            seps <- seps + ","
        while this.cur.Text.Length > 1 || seps.IndexOf(this.cur.Text) < 0 do
            list.Add(this.cur.Text)
            this.MoveNext()
        if list.Count < 1 then
            raise <| this.Abort("missing type or name")
        let mutable last = list.Count - 1
        let mutable name = list.[last]
        list.RemoveAt(last)
        if list.Count > 0 then
            let mutable t = Converter.ConvType(String.Concat(list.ToArray()))
            new TypeName(t, name)
        else
            new TypeName(null, name)

    member private this.ReadArg() =
        let mutable tn = this.ReadDecl(true)
        if tn.Type = null then
            raise <| this.Abort("missing type or name")
        Debug.Write(tn.Name + " : " + tn.Type)

    member private this.ReadBlock() =
        if this.cur.Text <> "{" then
            raise <| this.Abort("block required")
        this.MoveNext()
        if this.cur.Text = "}" then
            Debug.Write(this.indent)
            Debug.WriteLine("()")
        else
            while this.cur <> this.last && this.cur.Text <> "}" do
                this.ReadSentence()
        this.MoveNext()

    member private this.ReadSentence() =
        match this.cur.Text with
        | "return" ->
            this.MoveNext()
            this.ReadSentence()
        | "if" ->
            this.ReadIf()
        | "while" ->
            this.ReadWhile()
        | "switch" ->
            this.ReadSwitch()
        | "foreach" ->
            this.ReadForEach()
        | "continue"
        | "break" ->
            raise <| this.Abort("not supported")
        | "throw" ->
            this.MoveNext()
            Debug.Write(this.indent)
            Debug.Write("raise <| ")
            this.ReadExpr(false)
            Debug.WriteLine()
        | "var" ->
            this.ReadVar()
        | _ ->
            Debug.Write(this.indent)
            this.ReadExpr(false)
            Debug.WriteLine()

    member private this.ReadExpr(array : bool) =
        let mutable seps = ");:"
        if array then
            seps <- ",}"
            if seps.IndexOf(this.cur.Text) >= 0 then
                raise <| this.Abort("element required")
        while this.cur.Text.Length > 1 || seps.IndexOf(this.cur.Text) < 0 do
            let mutable t = this.cur.Text
            if t = "(" then
                this.isNew <- false
                this.MoveNext()
                Debug.Write("(")
                this.ReadExpr(false)
                Debug.Write(")")
            elif t = "," then
                this.MoveNext()
                Debug.Write(", ")
            elif t = "new" then
                this.MoveNext()
                if this.cur.Text = "[" then
                    this.ReadArray()
                else
                    Debug.Write("new ")
                    this.isNew <- true
            elif t = "delegate" then
                this.ReadDelegate()
            elif t = "." || this.cur.Type <> TokenType.Operator then
                this.MoveNext()
                Debug.Write("{0}", t)
            elif t = "!" then
                this.MoveNext()
                Debug.Write("not ")
            elif t = "~" then
                this.MoveNext()
                Debug.Write("~~~")
            elif Converter.noop.Contains(t) then
                raise <| this.Abort("not supported")
            elif this.isNew || t = "]" then
                this.MoveNext()
                Debug.Write(t)
            elif t = "[" then
                this.MoveNext()
                Debug.Write(".[")
            else
                this.MoveNext()
                Debug.Write(" " + Converter.ConvOp(t) + " ")
        if not array then
            this.MoveNext()

    member private this.ReadBlockOrExpr() =
        if this.cur.Text = ";" then
            this.MoveNext()
            Debug.Write("()")
        elif this.cur.Text = "{" then
            this.ReadBlock()
        else
            this.ReadSentence()

    member private this.ReadIf() =
        this.MoveNext()
        Debug.Write(this.indent)
        Debug.Write("if ")
        this.ReadIfInternal()

    member private this.ReadIfInternal() =
        if this.cur.Text <> "(" then
            raise <| this.Abort("must be '('")
        this.MoveNext()
        this.ReadExpr(false)
        Debug.WriteLine(" then")
        let mutable bak = this.indent
        this.indent <- this.indent + "    "
        this.ReadBlockOrExpr()
        if this.cur.Text = "else" then
            this.MoveNext()
            Debug.Write(bak)
            if this.cur.Text = "if" then
                this.indent <- bak
                this.MoveNext()
                Debug.Write("elif ")
                this.ReadIfInternal()
            else
                Debug.WriteLine("else")
                this.ReadBlockOrExpr()
                this.indent <- bak
        else
            this.indent <- bak

    member private this.ReadWhile() =
        this.MoveNext()
        Debug.Write(this.indent)
        Debug.Write("while ")
        if this.cur.Text <> "(" then
            raise <| this.Abort("must be '('")
        this.MoveNext()
        this.ReadExpr(false)
        Debug.Write(" do")
        if this.cur.Text = ";" then
            this.MoveNext()
            Debug.WriteLine(" ()")
        else
            Debug.WriteLine()
            let mutable bak = this.indent
            this.indent <- this.indent + "    "
            this.ReadBlockOrExpr()
            this.indent <- bak

    member private this.ReadForEach() =
        this.MoveNext()
        Debug.Write(this.indent)
        if this.cur.Text <> "(" then
            raise <| this.Abort("must be '('")
        this.MoveNext()
        if this.cur.Text <> "var" then
            raise <| this.Abort("must be 'var'")
        this.MoveNext()
        Debug.Write("for {0} in ", this.cur.Text)
        this.MoveNext()
        if this.cur.Text <> "in" then
            raise <| this.Abort("must be 'in'")
        this.MoveNext()
        this.ReadExpr(false)
        Debug.WriteLine(" do")
        let mutable bak = this.indent
        this.indent <- this.indent + "    "
        this.ReadBlockOrExpr()
        this.indent <- bak

    member private this.ReadSwitch() =
        this.MoveNext()
        Debug.Write(this.indent)
        Debug.Write("match ")
        if this.cur.Text <> "(" then
            raise <| this.Abort("must be '('")
        this.MoveNext()
        this.ReadExpr(false)
        Debug.WriteLine(" with")
        if this.cur.Text <> "{" then
            raise <| this.Abort("must be '{'")
        this.MoveNext()
        while this.cur.Text <> "}" do
            if this.cur.Text = "case" then
                while this.cur.Text = "case" do
                    this.MoveNext()
                    Debug.Write(this.indent)
                    Debug.Write("| ")
                    this.ReadExpr(false)
                    if this.cur.Text <> "case" then
                        Debug.Write(" ->")
                    else
                        Debug.WriteLine()
                this.ReadCaseBlock()
            elif this.cur.Text = "default" then
                this.MoveNext()
                Debug.Write(this.indent)
                Debug.Write("| _ ->")
                if this.cur.Text <> ":" then
                    raise <| this.Abort("must be ':'")
                this.MoveNext()
                this.ReadCaseBlock()
            else
                raise <| this.Abort("syntax error")
        this.MoveNext()

    member private this.ReadCaseBlock() =
        if this.cur.Text = "break" then
            Debug.WriteLine(" ()")
            this.MoveNext()
            if this.cur.Text <> ";" then
                raise <| this.Abort("must be ';'")
            this.MoveNext()
        else
            Debug.WriteLine()
            let mutable bak = this.indent
            this.indent <- this.indent + "    "
            while this.cur.Text <> "break" && this.cur.Text <> "return" && this.cur.Text <> "throw" do
                this.ReadSentence()
            if this.cur.Text = "return" then
                this.MoveNext()
                this.ReadSentence()
            elif this.cur.Text = "throw" then
                this.MoveNext()
                Debug.Write(this.indent)
                Debug.Write("raise <| ")
                this.ReadExpr(false)
                Debug.WriteLine()
            else
                this.MoveNext()
                if this.cur.Text <> ";" then
                    raise <| this.Abort("must be ';'")
                this.MoveNext()
            this.indent <- bak

    member private this.ReadVar() =
        this.MoveNext()
        if this.cur.Type <> TokenType.Any then
            raise <| this.Abort("name required")
        Debug.Write(this.indent)
        Debug.Write("let mutable {0} = ", this.cur.Text)
        this.MoveNext()
        if this.cur.Text <> "=" then
            raise <| this.Abort("must be '='")
        this.MoveNext()
        this.ReadExpr(false)
        Debug.WriteLine()

    member private this.ReadDelegate() =
        this.MoveNext()
        if this.cur.Text <> "(" then
            raise <| this.Abort("argument required")
        this.MoveNext()
        Debug.Write("(fun")
        while this.cur.Text <> ")" do
            let mutable tn = this.ReadDecl(true)
            Debug.Write(" ({0} : {1})", tn.Name, tn.Type)
            if this.cur.Text = "," then
                this.MoveNext()
        this.MoveNext()
        Debug.WriteLine(" ->")
        let mutable bak = this.indent
        this.indent <- this.indent + "    "
        this.ReadBlock()
        this.indent <- bak
        Debug.Write(this.indent)
        Debug.Write(")")

    member private this.ReadArray() =
        this.MoveNext()
        if this.cur.Text <> "]" then
            raise <| this.Abort("must be ']'")
        this.MoveNext()
        if this.cur.Text <> "{" then
            raise <| this.Abort("must be '{'")
        this.MoveNext()
        Debug.Write("[| ")
        while this.cur.Text <> "}" do
            this.ReadExpr(true)
            if this.cur.Text = "," then
                this.MoveNext()
                if this.cur.Text <> "}" then
                    Debug.Write("; ")
        this.MoveNext()
        Debug.Write(" |]")

    member private this.Abort(message : string) : Exception =
        new Exception(String.Format("[{0}, {1}] {2}: {3}", this.cur.Line, this.cur.Column, message, this.cur.Text))

    static member ConvType(t : string) : string =
        match t with
        | "uint" ->
            "uint32"
        | "short" ->
            "int16"
        | "ushort" ->
            "uint16"
        | "long" ->
            "int64"
        | "ulong" ->
            "uint64"
        | _ ->
            t

    static member ConvOp(op : string) : string =
        match op with
        | "=" ->
            "<-"
        | "==" ->
            "="
        | "!=" ->
            "<>"
        | ">>" ->
            ">>>"
        | "<<" ->
            "<<<"
        | "&" ->
            "&&&"
        | "|" ->
            "|||"
        | "^" ->
            "^^^"
        | _ ->
            op

    static member IsAccess(s : string) : bool =
        s = "public" || s = "protected" || s = "private"
