module type S = sig
  type addr

  val make_env :
    addr Request.t -> fullpath:string -> path:string -> string array
end

module Make (Addr : Types.ADDR) : S with type addr := Addr.t = struct
  let make_env req ~fullpath ~path =
    let empty = "" in
    let empty_by_default = Option.value ~default:"" in
    let common_name_cert =
      match Request.client_cert req with
      | [] -> None
      | c :: _ ->
          (* We pick the first one. *)
          Option.map
            (fun (_, d) -> Domain_name.to_string d)
            (X509.Certificate.hostnames c |> X509.Host.Set.choose_opt)
    in
    let client_addr = Format.asprintf "%a" Addr.pp (Request.ip req) in
    [|
      ( "AUTH_TYPE",
        Option.fold common_name_cert ~none:"" ~some:(fun _ -> "CERTIFICATE") );
      ("CONTENT_LENGTH", empty);
      ("CONTENT_TYPE", empty);
      ("GATEWAY_INTERFACE", "CGI/1.1");
      ("PATH_INFO", Request.target req |> Uri.pct_decode);
      ("PATH_TRANSLATED", path);
      ("QUERY_STRING", Request.query req |> empty_by_default);
      ("REMOTE_ADDR", client_addr);
      ("REMOTE_HOST", client_addr);
      ("REMOTE_IDENT", empty);
      ("REQUEST_METHOD", empty);
      ("REMOTE_USER", empty_by_default common_name_cert);
      ("SCRIPT_NAME", fullpath);
      ("SERVER_NAME", Request.uri req |> Uri.host |> empty_by_default);
      ("SERVER_PORT", Request.port req |> Int.to_string);
      ("SERVER_PROTOCOL", "GEMINI");
      ("SERVER_SOFTWARE", "Mehari/%%VERSION%%");
    |]
    |> Array.map (fun (name, value) -> Printf.sprintf "%s=%s" name value)
end
