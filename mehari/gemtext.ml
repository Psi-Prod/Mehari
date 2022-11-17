type t = line list

and line =
  | Text of string
  | Link of { url : string; name : string option }
  | Preformat of { alt : string option; text : string }
  | Heading of [ `H1 | `H2 | `H3 ] * string
  | ListItem of string
  | Quote of string

let text t = Text t
let link ?name url = Link { url; name }
let preformat ?alt text = Preformat { alt; text }
let heading h text = Heading (h, text)
let list_item text = ListItem text
let quote text = Quote text

let to_string lines =
  List.map
    (function
      | Text t -> t
      | Link { url; name } ->
          Option.fold name ~none:"" ~some:(Printf.sprintf " %s")
          |> Printf.sprintf "=> %s%s" url
      | Preformat { alt; text } ->
          Printf.sprintf "```%s\n%s\n```" (Option.value alt ~default:"") text
      | Heading (`H1, text) -> Printf.sprintf "# %s" text
      | Heading (`H2, text) -> Printf.sprintf "## %s" text
      | Heading (`H3, text) -> Printf.sprintf "### %s" text
      | ListItem text -> Printf.sprintf "* %s" text
      | Quote text -> Printf.sprintf ">%s" text)
    lines
  |> String.concat "\n"
