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

let pp_line ppf =
  let open Format in
  function
  | Text t -> pp_print_string ppf t
  | Link { url; name } ->
      fprintf ppf "=> %s%a" url (pp_print_option (Fun.flip fprintf " %s")) name
  | Preformat { alt; text } ->
      fprintf ppf "```%a@\n%s@\n```" (pp_print_option pp_print_string) alt text
  | Heading (`H1, t) -> fprintf ppf "# %s" t
  | Heading (`H2, t) -> fprintf ppf "## %s" t
  | Heading (`H3, t) -> fprintf ppf "### %s" t
  | ListItem t -> fprintf ppf "* %s" t
  | Quote t -> fprintf ppf ">%s" t

let pp ppf t =
  Format.pp_print_list ~pp_sep:Format.pp_force_newline pp_line ppf t

let to_string t = Format.asprintf "%a" pp t

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

type line_feed = LF | CRLF | EOF

(* Preserve line feed information to not erase it in pre-formatted blocks. *)
let show = function LF -> "\n" | CRLF -> "\r\n" | EOF -> ""

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
            let text =
              match Buffer.contents buf with
              | "" -> ""
              | s -> String.sub s 0 (String.length s - 1)
            in
            Buffer.reset buf;
            loop (Preformat { alt; text } :: acc) false None ls
        | true, false ->
            let alt =
              match String.sub l 3 (String.length l - 3) with
              | "" -> None
              | alt -> Some alt
            in
            loop acc true alt ls
        | false, true ->
            Buffer.add_string buf l;
            Buffer.add_string buf (show feed);
            loop acc is_preformat alt ls
        | false, false when l = "" -> loop (Text "" :: acc) is_preformat alt ls
        | false, false ->
            let line =
              try
                let grp = Re.exec Regex.h3 l in
                Heading (`H3, Re.Group.get grp 1)
              with Not_found -> (
                try
                  let grp = Re.exec Regex.h2 l in
                  Heading (`H2, Re.Group.get grp 1)
                with Not_found -> (
                  try
                    let grp = Re.exec Regex.h1 l in
                    Heading (`H1, Re.Group.get grp 1)
                  with Not_found -> (
                    try
                      let grp = Re.exec Regex.item l in
                      ListItem (Re.Group.get grp 1)
                    with Not_found -> (
                      try
                        let grp = Re.exec Regex.quote l in
                        Quote (Re.Group.get grp 1)
                      with Not_found -> (
                        try
                          let grp = Re.exec Regex.link l in
                          let url, name =
                            (Re.Group.get grp 1, Re.Group.get_opt grp 2)
                          in
                          Link { url; name }
                        with Not_found -> Text l)))))
            in
            loop (line :: acc) is_preformat alt ls)
  in
  split_lines text |> loop [] false None

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
