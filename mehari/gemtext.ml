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
  let space = Re.(alt [ char ' '; char '\t' ])

  let line prefix =
    Re.compile Re.(seq [ bol; prefix; rep space; group (rep1 any) ])

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
            rep space;
            group (rep1 (compl [ space ]));
            opt (seq [ rep space; group (rep1 any) ]);
          ])
end

type line_feed = LF | CRLF | EOF

(* Preserve line feed information for pre-formatted blocks. *)
let show = function LF -> "\n" | CRLF -> "\r\n" | EOF -> ""

(* Ugly but only one traversal. *)
let map_lines f text =
  let buf = Buffer.create 8192 in
  let len = String.length text - 1 in
  let rec loop n acc cr is_preformat alt =
    let n = n + 1 in
    if n > len then (
      if cr then Buffer.add_char buf '\r';
      let line, _, _ = f (Buffer.contents buf) EOF is_preformat alt in
      Option.fold line ~none:acc ~some:(fun l -> l :: acc))
    else
      match String.unsafe_get text n with
      | '\r' -> loop n acc true is_preformat alt
      | '\n' when cr ->
          let content = Buffer.contents buf in
          Buffer.reset buf;
          let line, is_preformat, alt = f content CRLF is_preformat alt in
          loop n
            (Option.fold line ~none:acc ~some:(fun l -> l :: acc))
            false is_preformat alt
      | '\n' ->
          let content = Buffer.contents buf in
          Buffer.reset buf;
          let line, is_preformat, alt = f content LF is_preformat alt in
          loop n
            (Option.fold line ~none:acc ~some:(fun l -> l :: acc))
            false is_preformat alt
      | c when cr ->
          Buffer.add_char buf '\r';
          Buffer.add_char buf c;
          loop n acc false is_preformat alt
      | c ->
          Buffer.add_char buf c;
          loop n acc false is_preformat alt
  in
  loop (-1) [] false false None

let of_string =
  let pf_buf = Buffer.create 4096 in
  map_lines (fun l feed is_preformat alt ->
      match (String.starts_with ~prefix:"```" l, is_preformat) with
      | true, true ->
          let text = Buffer.contents pf_buf in
          Buffer.reset pf_buf;
          (Some (Preformat { alt; text }), false, None)
      | true, false ->
          let alt =
            match String.sub l 3 (String.length l - 3) with
            | "" -> None
            | alt -> Some alt
          in
          (None, true, alt)
      | false, true ->
          Buffer.add_string pf_buf l;
          Buffer.add_string pf_buf (show feed);
          (None, is_preformat, alt)
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
          (Some frgmt, is_preformat, alt))

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
