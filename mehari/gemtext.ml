type t = line list

and line =
  | Text of string
  | Link of { url : string; name : string option }
  | Preformat of preformat
  | Heading of [ `H1 | `H2 | `H3 ] * string
  | List_item of string
  | Quote of string

and preformat = { alt : string option; text : string }

let text t = Text t
let newline = Text ""
let link ?name url = Link { url; name }
let preformat ?alt text = Preformat { alt; text }
let heading h text = Heading (h, text)
let list_item text = List_item text
let quote text = Quote text

let line_to_string = function
  | Text t -> t
  | Link { url; name } ->
      Printf.sprintf "=> %s%s" url
        (Option.fold ~none:"" ~some:(Printf.sprintf " %s") name)
  | Preformat { alt; text } ->
      let alt = Option.value ~default:"" alt in
      Printf.sprintf "```%s\n%s\n```" alt text
  | Heading (`H1, t) -> Printf.sprintf "# %s" t
  | Heading (`H2, t) -> Printf.sprintf "## %s" t
  | Heading (`H3, t) -> Printf.sprintf "### %s" t
  | List_item t -> Printf.sprintf "* %s" t
  | Quote t -> Printf.sprintf ">%s" t

let to_string lines = lines |> List.map line_to_string |> String.concat "\n"
let pp_line ppf line = Format.pp_print_string ppf @@ line_to_string line
let pp ppf lines = Format.pp_print_string ppf @@ to_string lines

let paragraph gemtext s =
  let doc = ref [] in
  let cr = ref false in
  let buf = Buffer.create (String.length s) in
  String.iteri
    (fun i -> function
      | '\r' -> cr := true
      | '\n' when !cr ->
          let line = Buffer.contents buf in
          Buffer.reset buf;
          doc := gemtext line :: !doc;
          cr := false
      | '\n' ->
          let line = Buffer.contents buf in
          Buffer.reset buf;
          doc := gemtext line :: !doc;
          cr := false
      | c ->
          if !cr then Buffer.add_char buf '\r';
          Buffer.add_char buf c;
          cr := false)
    s;
  List.rev
  @@ match Buffer.contents buf with "" -> !doc | line -> gemtext line :: !doc

module Parser = struct
  open Angstrom

  let is_space = function ' ' | '\t' -> true | _ -> false
  let spaces = skip_while is_space

  let eol =
    string "\r\n" <|> string "\r" <|> string "\n" <|> end_of_input *> return ""

  let until_eol = take_till (function '\r' | '\n' -> true | _ -> false) <* eol

  let until_eol_feed =
    let line = take_till (function '\r' | '\n' -> true | _ -> false) in
    both line eol

  let text = until_eol >>| text

  let heading lvl =
    let prefix =
      String.make (match lvl with `H1 -> 1 | `H2 -> 2 | `H3 -> 3) '#'
    in
    let+ text = string prefix *> spaces *> until_eol in
    Heading (lvl, text)

  let list_item = string "* " *> until_eol >>| list_item
  let quote = char '>' *> until_eol >>| quote

  let link =
    let+ raw_link = string "=>" *> spaces *> until_eol in
    let url, raw_name = String.cut_first_while (Fun.negate is_space) raw_link in
    let name =
      match String.drop_first_while is_space raw_name with
      | "" -> None
      | name -> Some name
    in
    Link { url; name }

  let close_preformat =
    let* line = until_eol in
    if String.starts_with ~prefix:"```" line then return ()
    else fail "not a closing fence"

  let join lines =
    let len = List.length lines in
    let lines =
      List.mapi
        (fun i (line, feed) -> if i < len - 1 then line ^ feed else line)
        lines
    in
    String.concat "" lines

  let preformat =
    let* alt =
      string "```" *> until_eol >>| function "" -> None | alt -> Some alt
    in
    let+ lines = many_till until_eol_feed close_preformat in
    Preformat { text = join lines; alt }

  let line =
    preformat <|> heading `H3 <|> heading `H2 <|> heading `H1 <|> link
    <|> list_item <|> quote <|> text

  let gemtext = many_till line end_of_input
end

let of_string s =
  match Angstrom.parse_string ~consume:Prefix Parser.gemtext s with
  | Ok lines ->
      if String.ends_with ~suffix:"\n" s then lines @ [ text "" ] else lines
  | Error msg -> failwith msg

let equal_line l l' =
  match (l, l') with
  | Text t, Text t' -> String.equal t t'
  | Link l, Link l' ->
      String.equal l.url l'.url && Option.equal String.equal l.name l'.name
  | Preformat p, Preformat p' ->
      Option.equal String.equal p.alt p'.alt && String.equal p.text p'.text
  | Heading (`H1, h), Heading (`H1, h')
  | Heading (`H2, h), Heading (`H2, h')
  | Heading (`H3, h), Heading (`H3, h') ->
      String.equal h h'
  | List_item i, List_item i' -> String.equal i i'
  | Quote q, Quote q' -> String.equal q q'
  | _ -> false

let equal = List.equal equal_line
