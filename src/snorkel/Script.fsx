#r "../../packages/FSharp.Formatting/lib/net40/FSharp.Markdown.dll"
open FSharp.Markdown

#load "HtmlFormatting.fs"
open snorkel.Html
let document = """
# Example Project Report

Overview of project [Bob](ns:proj#bob 'project name') [written on 29th Feb, 2016](ns:compiled-date#2016-02-29 'compiled on')

[became redundant on](ns:date#2016-02-2 'redundant on')


[written on 29th Feb, 2016](ns:date#2016-02-2 'compiled on')

"""

let parsed = Markdown.Parse(document)

let splitBy f input =
  let i = ref 0
  input
  |> Seq.groupBy (fun x ->
    if f x then incr i
    !i)
  |> Seq.map snd

type Id = | Id of string


let grouped = parsed.Paragraphs |> splitBy (function | Heading(_,_) -> true | _ -> false)

type Link = {
  text:string
  uri:string
  title:string option
  }

/// Returns all links in a specified span node
let rec collectSpanLinks span = seq {
  match span with
  | DirectLink([Literal(text)], (uri, title)) -> yield {text = text; uri = uri; title = title;}
  | IndirectLink(_, _, key) -> () //yield fst (parsed.DefinedLinks.[key])
  | Matching.SpanLeaf _ -> ()
  | Matching.SpanNode(_, spans) ->
      for s in spans do yield! collectSpanLinks s }

/// Returns all links in the specified paragraph node
let rec collectParLinks par = seq {
  match par with
  | Matching.ParagraphLeaf _ -> ()
  | Matching.ParagraphNested(_, pars) ->
      for ps in pars do
        for p in ps do yield! collectParLinks p
  | Matching.ParagraphSpans(_, spans) ->
      for s in spans do yield! collectSpanLinks s }

/// Collect links in the entire document
let links = Seq.collect collectParLinks parsed.Paragraphs

let html = ExtMarkdown.WriteHtml(parsed)
