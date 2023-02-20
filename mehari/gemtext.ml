type t = line list

and line =
  | Text of string
  | Link of { url : string; name : string option }
  | Preformat of preformat
  | Heading of [ `H1 | `H2 | `H3 ] * string
  | ListItem of string
  | Quote of string

and preformat = { alt : string option; text : string }

let text t = Text t
let newline = Text ""
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

module Regex = struct
  let spaces = Re.(rep (alt [ char ' '; char '\t' ]))

  let line prefix =
    Re.compile Re.(seq [ bol; prefix; spaces; group (rep1 any) ])

  let h1 = line (Re.char '#')
  let h2 = line (Re.str "##")
  let h3 = line (Re.str "###")
  let item = line (Re.str "* ")
  let quote = Re.compile Re.(seq [ bol; Re.char '>'; group (rep1 any) ])

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

type line_feed = LF | CRLF | EOF

(* Preserve line feed information to not erase it in pre-formatted blocks. *)
let show = function LF -> "\n" | CRLF -> "\r\n" | EOF -> ""

let split_lines text =
  let buf = Buffer.create 8192 in
  let acc = ref [] in
  let cr = ref false in
  for i = 0 to String.length text - 1 do
    match String.unsafe_get text i with
    | '\r' -> cr := true
    | '\n' when !cr ->
        let content = Buffer.contents buf in
        Buffer.reset buf;
        cr := false;
        acc := (content, CRLF) :: !acc
    | '\n' ->
        let content = Buffer.contents buf in
        Buffer.reset buf;
        acc := (content, LF) :: !acc
    | c when !cr ->
        cr := false;
        Buffer.add_char buf '\r';
        Buffer.add_char buf c
    | c -> Buffer.add_char buf c
  done;
  if !cr then Buffer.add_char buf '\r';
  acc := (Buffer.contents buf, EOF) :: !acc;
  List.rev !acc

let of_string text =
  let buf = Buffer.create 4096 in
  let rec loop acc is_preformat alt = function
    | [] -> List.rev acc
    | (l, feed) :: ls -> (
        match (String.starts_with ~prefix:"```" l, is_preformat) with
        | true, true ->
            let text = Buffer.contents buf in
            Buffer.reset buf;
            let alt =
              match String.sub l 3 (String.length l - 3) with
              | "" -> None
              | alt -> Some alt
            in
            loop (Preformat { alt; text } :: acc) false None ls
        | true, false -> loop acc true alt ls
        | false, true ->
            Buffer.add_string buf l;
            Buffer.add_string buf (show feed);
            loop acc is_preformat alt ls
        | false, false ->
            let frgmt =
              if l = "" then Text ""
              else
                match Re.exec_opt Regex.h3 l with
                | Some grp -> Heading (`H3, Re.Group.get grp 1)
                | None -> (
                    match Re.exec_opt Regex.h2 l with
                    | Some grp -> Heading (`H2, Re.Group.get grp 1)
                    | None -> (
                        match Re.exec_opt Regex.h1 l with
                        | Some grp -> Heading (`H1, Re.Group.get grp 1)
                        | None -> (
                            match Re.exec_opt Regex.item l with
                            | Some grp -> ListItem (Re.Group.get grp 1)
                            | None -> (
                                match Re.exec_opt Regex.quote l with
                                | Some grp -> Quote (Re.Group.get grp 1)
                                | None -> (
                                    match Re.exec_opt Regex.link l with
                                    | None -> Text l
                                    | Some grp ->
                                        let url, name =
                                          ( Re.Group.get grp 1,
                                            Re.Group.get_opt grp 2 )
                                        in
                                        Link { url; name })))))
            in
            loop (frgmt :: acc) is_preformat alt ls)
  in
  split_lines text |> loop [] false None

let paragraph gemtext str =
  let doc = ref [] in
  let cr = ref false in
  let buf = Buffer.create 4096 in
  for i = 0 to String.length str - 1 do
    match String.unsafe_get str i with
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
  (match Buffer.contents buf with "" -> !doc | line -> gemtext line :: !doc)
  |> List.rev

let pp fmt g = Format.fprintf fmt "%s" (to_string g)
