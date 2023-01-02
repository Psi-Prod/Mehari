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

let of_string text =
  let rec loop acc is_preformat pf = function
    | [] -> List.rev acc
    | x :: xs -> (
        match (String.starts_with ~prefix:"```" x, is_preformat) with
        | true, true ->
            loop (Preformat pf :: acc) (not is_preformat)
              { alt = None; text = "" } xs
        | true, false ->
            let alt_str = String.sub x 3 (String.length x - 3) in
            let alt = if alt_str = "" then None else Some alt_str in
            loop acc (not is_preformat) { pf with alt } xs
        | false, true ->
            loop acc is_preformat { pf with text = pf.text ^ x ^ "\n" } xs
        | false, false ->
            let frgmt =
              if x = "" then Text ""
              else
                match Re.exec_opt Regex.h3 x with
                | Some grp -> Heading (`H3, Re.Group.get grp 1)
                | None -> (
                    match Re.exec_opt Regex.h2 x with
                    | Some grp -> Heading (`H2, Re.Group.get grp 1)
                    | None -> (
                        match Re.exec_opt Regex.h1 x with
                        | Some grp -> Heading (`H1, Re.Group.get grp 1)
                        | None -> (
                            match Re.exec_opt Regex.item x with
                            | Some grp -> ListItem (Re.Group.get grp 1)
                            | None -> (
                                match Re.exec_opt Regex.quote x with
                                | Some grp -> Quote (Re.Group.get grp 1)
                                | None -> (
                                    match Re.exec_opt Regex.link x with
                                    | None -> Text x
                                    | Some grp ->
                                        let url, name =
                                          ( Re.Group.get grp 1,
                                            Re.Group.get_opt grp 2 )
                                        in
                                        Link { url; name })))))
            in
            loop (frgmt :: acc) is_preformat pf xs)
  in
  Re.(split (compile (alt [ char '\n'; str "\r\n" ]))) text
  |> loop [] false { alt = None; text = "" }

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
