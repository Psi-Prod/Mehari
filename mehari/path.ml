type 'a variable = {
  from_string : string -> 'a option;
  to_string : 'a -> string;
}

type (_, _) t =
  | Root : ('witness, 'witness) t
  | Const : ('continuation, 'witness) t * string -> ('continuation, 'witness) t
  | Var :
      ('continuation, 'new_variable -> 'witness) t * 'new_variable variable
      -> ('continuation, 'witness) t

type ('continuation, 'witness) wrapped = unit -> ('continuation, 'witness) t

let variable ~from_string ~to_string = { from_string; to_string }

let variable_from_string : type a. a variable -> string -> a option =
 fun fragment value ->
  match fragment with { from_string; _ } -> from_string value

let string = variable ~from_string:Option.some ~to_string:(fun x -> x)
let int = variable ~from_string:int_of_string_opt ~to_string:string_of_int
let float = variable ~from_string:float_of_string_opt ~to_string:string_of_float
let bool = variable ~from_string:bool_of_string_opt ~to_string:string_of_bool

let char =
  let is_char s = Int.equal 1 @@ String.length s in
  let from_string s = if is_char s then Some s.[0] else None in
  let to_string c = String.make 1 c in
  variable ~from_string ~to_string

let root = Root
let add_constant value base = Const (base, value)
let add_variable variable base = Var (base, variable)

module Infix = struct
  let ( ~/ ) value = add_constant value root
  let ( ~/: ) variable = add_variable variable root
  let ( / ) base value = add_constant value base
  let ( /: ) base variable = add_variable variable base
end

include Infix

module Private = struct
  module Href = struct
    type t = {
      fragments : string list;
      query_string : string option;
      anchor : string option;
    }

    let make ?query_string ?anchor fragments =
      { fragments; query_string; anchor }

    let fragments { fragments; _ } = fragments

    let extract_at chr str =
      match String.split_on_char chr str with
      | [ ""; "" ] -> (None, None)
      | [ ""; x ] -> (None, Some x)
      | [ x; "" ] -> (Some x, None)
      | [ x; y ] -> (Some x, Some y)
      | [ "" ] -> (None, None)
      | [ x ] -> (Some x, None)
      | _ -> (None, None)

    let extract_anchor = extract_at '#'

    let extract_query_string = function
      | None -> (None, None)
      | Some tl -> extract_at '?' tl

    let split_fragments str =
      match String.split_on_char '/' str with
      | "" :: "" :: fragments | "" :: fragments | fragments -> fragments

    let extract_fragments = function None -> [] | Some x -> split_fragments x

    let from_string str =
      let tl, anchor = extract_anchor str in
      let tl, query_string = extract_query_string tl in
      let fragments = extract_fragments tl in
      make ?query_string ?anchor fragments
  end

  let sscanf path uri =
    let rec aux : type continuation witness normal_form.
        (witness -> normal_form) ->
        (continuation, witness) t ->
        string list ->
        continuation ->
        normal_form option =
     fun continue path fragments ->
      match (path, fragments) with
      | Root, [] -> fun x -> Some (continue x)
      | Const (path_xs, x), fragment :: uri_xs ->
          if String.equal x fragment then aux continue path_xs uri_xs
          else fun _ -> None
      | Var (path_xs, fr), fragment :: uri_xs ->
          Option.fold
            ~none:(fun _ -> None)
            ~some:(fun var ->
              aux (fun acc -> continue (acc var)) path_xs uri_xs)
          @@ variable_from_string fr fragment
      | _ -> fun _ -> None
    in
    let parsed = Href.from_string uri in
    let fragments = List.rev @@ Href.fragments parsed in
    aux (fun x -> x) path fragments
end
