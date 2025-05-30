type t = {
  client_ip : Ipaddr.t;
  client_port : int;
  port : int;
  uri : Uri.t;
  server_hostname : string;
  client_cert : X509.Certificate.t option;
  tls_version : [ `TLS_1_2 | `TLS_1_3 ];
}

let uri { uri; _ } = uri
let target { uri; _ } = Uri.path uri
let ip { client_ip; _ } = client_ip
let port { port; _ } = port
let query { uri; _ } = Uri.verbatim_query uri
let client_cert { client_cert; _ } = client_cert
let tls_version { tls_version; _ } = tls_version

let make ?client_cert ~uri ~client_ip ~client_port ~port ~server_hostname
    ~tls_version () =
  {
    uri;
    client_ip;
    client_port;
    port;
    server_hostname;
    client_cert;
    tls_version;
  }

module Private = struct
  let make = make
  let client_port r = r.client_port
  let server_hostname r = r.server_hostname
end
