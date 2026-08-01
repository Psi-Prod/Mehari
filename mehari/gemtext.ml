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
  for i = 0 to String.length s - 1 do
    match String.unsafe_get s i with
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
        cr := false
  done;
  List.rev
  @@ match Buffer.contents buf with "" -> !doc | line -> gemtext line :: !doc

module Regex = struct
  let spaces = Re.(rep (alt [ char ' '; char '\t' ]))

  let line prefix =
    Re.compile Re.(seq [ bol; prefix; spaces; group (rep1 any) ])

  let h1 = line (Re.char '#')
  let h2 = line (Re.str "##")
  let h3 = line (Re.str "###")
  let item = line (Re.str "* ")
  let quote = Re.compile Re.(seq [ bol; Re.char '>'; group (rep any) ])

  let link =
    Re.compile
      Re.(
        seq
          [
            str "=>";
            spaces;
            group (rep1 (compl [ space ]));
            opt (seq [ spaces; group (rep1 any) ]);
          ])
end

type line_feed = LF | CRLF

(* Preserve line feed information to not erase it in pre-formatted blocks. *)
let string_of_line_feed = function
  | None -> ""
  | Some LF -> "\n"
  | Some CRLF -> "\r\n"

let split_lines text =
  let buf = Buffer.create 8192 in
  let acc = ref [] in
  let cr = ref false in
  for i = 0 to String.length text - 1 do
    match String.unsafe_get text i with
    | '\r' when !cr -> Buffer.add_char buf '\r'
    | '\r' -> cr := true
    | '\n' when !cr ->
        let content = Buffer.contents buf in
        Buffer.reset buf;
        cr := false;
        acc := (content, Some CRLF) :: !acc
    | '\n' ->
        let content = Buffer.contents buf in
        Buffer.reset buf;
        acc := (content, Some LF) :: !acc
    | c when !cr ->
        cr := false;
        Buffer.add_char buf '\r';
        Buffer.add_char buf c
    | c -> Buffer.add_char buf c
  done;
  if !cr then Buffer.add_char buf '\r';
  acc := (Buffer.contents buf, None) :: !acc;
  List.rev !acc

let of_string text =
  let rec loop acc preformat = function
    | [] -> List.rev acc
    | (line, feed) :: lines -> (
        match (String.starts_with ~prefix:"```" line, preformat) with
        | true, Some (alt, preformat) ->
            let text =
              match Buffer.contents preformat with
              | "" -> ""
              | s -> String.sub s 0 (String.length s - 1)
            in
            loop (Preformat { alt; text } :: acc) None lines
        | true, None ->
            let alt =
              match String.sub line 3 (String.length line - 3) with
              | "" -> None
              | alt -> Some alt
            in
            loop acc (Some (alt, Buffer.create 4096)) lines
        | false, (Some (_, preformat) as pf) ->
            Buffer.add_string preformat line;
            Buffer.add_string preformat (string_of_line_feed feed);
            loop acc pf lines
        | false, None ->
            if line = "" then loop (Text "" :: acc) preformat lines
            else
              let line =
                try
                  let grp = Re.exec Regex.h3 line in
                  Heading (`H3, Re.Group.get grp 1)
                with Not_found -> (
                  try
                    let grp = Re.exec Regex.h2 line in
                    Heading (`H2, Re.Group.get grp 1)
                  with Not_found -> (
                    try
                      let grp = Re.exec Regex.h1 line in
                      Heading (`H1, Re.Group.get grp 1)
                    with Not_found -> (
                      try
                        let grp = Re.exec Regex.item line in
                        List_item (Re.Group.get grp 1)
                      with Not_found -> (
                        try
                          let grp = Re.exec Regex.quote line in
                          Quote (Re.Group.get grp 1)
                        with Not_found -> (
                          try
                            let grp = Re.exec Regex.link line in
                            let url, name =
                              (Re.Group.get grp 1, Re.Group.get_opt grp 2)
                            in
                            Link { url; name }
                          with Not_found -> Text line)))))
              in
              loop (line :: acc) preformat lines)
  in
  split_lines text |> loop [] None

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
