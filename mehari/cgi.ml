module type S = sig
  type addr

  val make_env :
    addr Request.t -> fullpath:string -> path:string -> string array
end

module Make (Addr : Types.ADDR) : S with type addr := Addr.t = struct
  let make_env req ~fullpath ~path =
    let empty = "" in
    [|
      ("AUTH_TYPE", "CERTIFICATE");
      ("CONTENT_LENGTH", empty);
      ("CONTENT_TYPE", empty);
      ("GATEWAY_INTERFACE", "CGI/1.1");
      ("PATH_INFO", Request.uri req |> Uri.path |> Uri.pct_decode);
      ("PATH_TRANSLATED", path);
      ("QUERY_STRING", Request.query req |> Option.value ~default:"");
      ("REMOTE_ADDR", Format.kasprintf Fun.id "%a" Addr.pp (Request.ip req));
      ("REMOTE_HOST", Request.uri req |> Uri.host |> Option.value ~default:"");
      ("REMOTE_IDENT", empty);
      ("REQUEST_METHOD", empty);
      ("SCRIPT_NAME", fullpath);
      ("SERVER_NAME", Request.uri req |> Uri.host |> Option.value ~default:"");
      ("SERVER_PORT", Request.port req |> Int.to_string);
      ("SERVER_PROTOCOL", "GEMINI");
      ("SERVER_SOFTWARE", "Mehari/%%VERSION%%");
    |]
    |> Array.map (fun (name, value) -> Printf.sprintf "%s=%s" name value)
end
