type t = {
  client_ip : Ipaddr.t;
  port : int;
  uri : Uri.t;
  server_hostname : string;
  params : Re.Group.t option;
  client_cert : X509.Certificate.t option;
  tls_version : [ `TLS_1_2 | `TLS_1_3 ];
}

let uri { uri; _ } = uri
let target { uri; _ } = Uri.path uri
let ip { client_ip; _ } = client_ip
let port { port; _ } = port
let server_hostname { server_hostname; _ } = server_hostname
let query { uri; _ } = Uri.verbatim_query uri
let client_cert { client_cert; _ } = client_cert
let tls_version { tls_version; _ } = tls_version

let make ?client_cert ~uri ~client_ip ~port ~server_hostname ~tls_version () =
  {
    uri;
    client_ip;
    port;
    server_hostname;
    params = None;
    client_cert;
    tls_version;
  }

let attach_params t params = { t with params }

let param t p =
  let fail () = invalid_arg "Mehari.param" in
  match t.params with
  | None -> fail ()
  | Some _ when p <= 0 -> fail ()
  | Some grp -> (
      match Re.Group.get_opt grp p with None -> fail () | Some param -> param)

module Private = struct
  let make = make
  let server_hostname = server_hostname
  let attach_params = attach_params
end
