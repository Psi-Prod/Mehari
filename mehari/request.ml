type t = {
  client_ip : Ipaddr.t;
  client_port : int;
  port : int;
  uri : Uri.t;
  server_hostname : hostname;
  client_cert : X509.Certificate.t option;
  tls_version : [ `TLS_1_2 | `TLS_1_3 ];
}

and hostname = Domain_name of [ `host ] Domain_name.t | Ip_addr of Ipaddr.t

let uri { uri; _ } = uri
let target { uri; _ } = Uri.path uri
let ip { client_ip; _ } = client_ip
let port { port; _ } = port
let query { uri; _ } = Uri.verbatim_query uri
let client_cert { client_cert; _ } = client_cert
let tls_version { tls_version; _ } = tls_version

let make ?client_cert ~uri ~client:(ip, client_port) ~port ~server_hostname
    ~tls_version () =
  {
    uri;
    client_ip = ip;
    client_port;
    port;
    server_hostname;
    client_cert;
    tls_version;
  }

module Private = struct
  type nonrec hostname = hostname =
    | Domain_name of [ `host ] Domain_name.t
    | Ip_addr of Ipaddr.t

  let make = make
  let client_port r = r.client_port
  let server_hostname r = r.server_hostname
end
