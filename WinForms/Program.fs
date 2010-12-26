open System
open System.Collections.Generic
open System.Drawing
open System.IO
open System.Windows.Forms
open CSharpParser

let f = new Form()
f.ClientSize <- Size(600, 400)
f.Text <- "C# to F# converter"

let sp = new SplitContainer();
sp.Dock <- DockStyle.Fill
sp.Orientation <- Orientation.Vertical
sp.SplitterDistance <- sp.Width / 2;
f.Controls.Add(sp)

let t1 = new TextBox()
t1.Dock <- DockStyle.Fill
t1.WordWrap <- false
t1.Multiline <- true
t1.ScrollBars <- ScrollBars.Both
sp.Panel1.Controls.Add(t1)

let t2 = new TextBox()
t2.Dock <- DockStyle.Fill
t2.WordWrap <- false
t2.Multiline <- true
t2.ScrollBars <- ScrollBars.Both
sp.Panel2.Controls.Add(t2)

let menu = new MainMenu()
let mi0 = new MenuItem("Paste &Source")
let mi1 = new MenuItem("&Convert")
let mi2 = new MenuItem("Copy &Result")
let mi3 = new MenuItem("&Exit")
menu.MenuItems.AddRange([| mi0; mi1; mi2; mi3 |])
f.Menu <- menu

mi0.Click.Add <| fun e ->
    if Clipboard.ContainsText() then
        t1.Text <- Clipboard.GetText()

mi1.Click.Add <| fun e ->
    Debug.Stream <- new StringWriter()
    t2.Clear()
    let mutable lex = new Lexer(t1.Text)
    let mutable tokens = lex.ReadAllTokens()
    let mutable conv = new Converter(tokens)
    conv.Convert()
    Debug.Stream.Close()
    t2.AppendText(Debug.Stream.ToString())

mi2.Click.Add <| fun e ->
    if t2.TextLength > 0 then
        Clipboard.SetText(t2.Text)

mi3.Click.Add <| fun e -> f.Close()

[<STAThread>]
Application.Run(f)
