open Mehari
open Mehari

let book =
  object
    val mutable entries = []

    method add_entry ~addr msg =
      entries <- (Mirage_ptime.now (), addr, msg) :: entries

    method print =
      let buf = Buffer.create 4096 in
      List.iter
        (fun (ptime, addr, msg) ->
          let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ptime in
          Format.kasprintf (Buffer.add_string buf)
            "%i-%i-%i %i:%i:%i - %a: %s\n" y m d hh mm ss Ipaddr.pp addr
            (Uri.pct_decode msg))
        entries;
      Buffer.contents buf
  end

let router =
  let open Router in
  router
    [
      route Path.root (fun _ ->
          Gemtext.
            [
              heading `H1 "Guestbook";
              newline;
              link "/submit" ~name:"Submit a new entry";
              newline;
              heading `H2 "Entries:";
              text book#print;
            ]
          |> Response.gemtext);
      route
        Path.(~/"submit")
        (fun req ->
          match Request.query req with
          | None -> Response.respond Status.input "Enter your message"
          | Some msg ->
              book#add_entry ~addr:(Request.ip req) msg;
              Response.respond Status.redirect_temp "/");
    ]

let certs =
  let certs =
    X509.Certificate.decode_pem_multiple
      {|-----BEGIN CERTIFICATE-----
MIIEAzCCAmugAwIBAgIQS7GKkpFXZC4+UhCQaXJsPDANBgkqhkiG9w0BAQsFADBV
MR4wHAYDVQQKExVta2NlcnQgZGV2ZWxvcG1lbnQgQ0ExFTATBgNVBAsMDGRvZ21A
ZG9nbS1wYzEcMBoGA1UEAwwTbWtjZXJ0IGRvZ21AZG9nbS1wYzAeFw0yNjA1MjAx
MTQ5MTRaFw0yODA4MjAxMTQ5MTRaMEAxJzAlBgNVBAoTHm1rY2VydCBkZXZlbG9w
bWVudCBjZXJ0aWZpY2F0ZTEVMBMGA1UECwwMZG9nbUBkb2dtLXBjMIIBIjANBgkq
hkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA3fPwEP1HEqaytjsom9M1DiyT1I0NHvqS
VaOF9zECeEML/IzLrZeUR4QGW7y//2qYEjXzfKRTB5DP7zPWzln4zX/MBkWt18jh
F8zY9LBbAnf1lX2T4NKmo/XiZGI93C2Kz+y7PckvQejTrrXM1BzWxKKTTi7Sx2GR
wE5zU8ykWLA3B6bz9DxkfXIWqYN8OWDlBg/y8EICXCev3eCbknYMbGKvS/14ff2k
mzGjwMJ8dYRDHOx0DR7Ymw0BhFw/6jlUCu6F4WxtnnZCFkTlWAgAs1mEVtkzO/Pm
QQdfsFOgi07jPSSmfUrPwlPcn2LC5efpcctN1odqfNLIdXolV/T1nQIDAQABo2Qw
YjAOBgNVHQ8BAf8EBAMCBaAwEwYDVR0lBAwwCgYIKwYBBQUHAwEwHwYDVR0jBBgw
FoAU0MEezskCI0nLpCWm4ea8Bw1A14YwGgYDVR0RBBMwEYIJbG9jYWxob3N0hwQK
AAACMA0GCSqGSIb3DQEBCwUAA4IBgQCcu3EZaKIXq4ZcgFHke3YKwYGy4fmS+0Mq
opi3qNHGHubytb2RbedI4r01SfwDvwmmGg2IUJPEh4h+mhaeHrHA/zsIiet2Z3fe
8nR9EBQ1rbuOdY4qbKDobK1QPr32ICyVphHCfskZf8RqHCLEa9x5B1smkJ9KfVWX
mCbmzV2n/qiCdGu49V616uCezJcTDrDBEQCTT6SK5RLR3qaHMZYu9OwTgCbZXJ3M
/2uRkmiFCMRXBn2St08m+x7HcbTWuxqnXpK3LyCoBRmkiaDJxen0Nb9XgCuQwvd5
AIh42Zf14/AwuZjNL2LnAvcVu1DHPD8r7eb8bHHGvomj2H81VX69n6Sbk1N+BbgR
cWQrHxlGUrVPJp+JoqWrb9bSNXYKRFuWSgRw5VUqDWaAmsfifd+2uxvH3v3vWwRJ
z8W/IEG1YStC79VWx3zH/cdxXJbXVtVWtBD+EIVZJ5LECEc9yBnI8TOfoH65nh/U
QQ5ki3HGj9T9jJ6GPT25JnacyssGUho=
-----END CERTIFICATE-----
|}
  and key =
    X509.Private_key.decode_pem
      {|-----BEGIN PRIVATE KEY-----
MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDd8/AQ/UcSprK2
Oyib0zUOLJPUjQ0e+pJVo4X3MQJ4Qwv8jMutl5RHhAZbvL//apgSNfN8pFMHkM/v
M9bOWfjNf8wGRa3XyOEXzNj0sFsCd/WVfZPg0qaj9eJkYj3cLYrP7Ls9yS9B6NOu
tczUHNbEopNOLtLHYZHATnNTzKRYsDcHpvP0PGR9chapg3w5YOUGD/LwQgJcJ6/d
4JuSdgxsYq9L/Xh9/aSbMaPAwnx1hEMc7HQNHtibDQGEXD/qOVQK7oXhbG2edkIW
ROVYCACzWYRW2TM78+ZBB1+wU6CLTuM9JKZ9Ss/CU9yfYsLl5+lxy03Wh2p80sh1
eiVX9PWdAgMBAAECggEAWRHXuTmrF/UFqtO6zC3KENIa0SV5qoDr0Ko6zKRCVXiy
irsF0bgOrVRyKMZqVlm0hTpZaB+ZomOezmODRHQewHkUqz+zL/FRQ3fVaPyMyUTS
rRckDQj8DrhSDe7O3Fj6Z86cpYLjEBQbPvV+KXEZTvffHXT06UZic0YISgzJtzzg
WcgNqdI/wAt3bmp6VFFahuiXlIOEjtxyYR8y4E20Z3/3qERTfg1gqKbl1QVXZSq1
x7A1h3WpPEDhblPcxbxtWpKR3NT/D/vhgo3qwVomURd6J0Fm0H7Nl8dZ9kdJsnxv
fly+NK5fSu+3jqZpaEfrx9tmPRx9tk9v88Ymj/LPkQKBgQDe9rYKQCRz332g6SCL
2c1uUAP08Zqhj06ubiFCu8LA4BR8at0W80SWHtBtMdTT8+L1eUUh7IavKAiurR5G
wKJaDHTdYz17Vnn7D2MHN3qGrNoObU+wHTL97u9GQ4v+N8/kEySYczjg2twIR8UU
YvcAdIIAhfTysANR4o4ChOZKhwKBgQD+1uJvMdEP8AnNOI+1FiWjaAdpme/Tjvg4
PgL6TvdO7HjhTrrNDuZjz/pwQuVId6RjsTkBTP0TWrNcT05fs+Bg6PHY7y9WN/5J
nKIeJN2b3hq9/nMkpPynKBzVHc6GoDeH9KgOLmQgQu/XyNIghuZHuKa5+nWg2X39
BoBDKMeTuwKBgAnw4gju9zEbY9SK0GKr4VbrxmFjbEuLwixQyLba/EHxO7iYYyck
zYgL86sKr7yLgTEEqSpsC00/8bMoENOCvzpYl9VPGWyoXGPvObEWriXXCxndkdH+
W0mrH4v4cwNwpZo7qEQLzxtnZqwP1x+jkfO5Phqb3mH7qwo5ma9PEZ19AoGBAJkc
4s+MTuAUiM8UvMNAmzyitFFue/nQXLLuDzv1VgHFcptsgi/SU/qEayQoDTqNJ+f+
AxIU/7kxNbKws5YsrROM4kJE81wKKG5x/bW+0KdluwuMZgIOXv7PywMA9aVNfg/h
n1dl6CK6FQDBV/7TDmjpOMNlCGesRVZRIZ84UTEhAoGAH7RC3jr8N0b8w72DflUV
pEsyfwU31QxLvNzBFU5cpIYLzswqtxgN5Wn61KwVPm4zkuMAVqoYAD3IGcICeyDh
/h1i9GHGHxUYwbvrgwFr0J3pnUEkCAWaYaET+vqDx0qIhK7eg761QvPlazmrn5sL
QGm4zErz8xb0WyEZP5APo4U=
-----END PRIVATE KEY-----
|}
  in
  [ (Result.get_ok certs, Result.get_ok key) ]

let devices ?gateway cidr =
  let open Mkernel in
  let rng =
    map
      (fun () ->
        Mirage_crypto_rng_mkernel.initialize (module Mirage_crypto_rng.Fortuna))
      []
  and net = Mnet.stack ~name:"service" ?gateway cidr in
  [ rng; net ]

let run cidr gateway port =
  Mkernel.run (devices ?gateway cidr) @@ fun rng (daemon, tcp, _) () ->
  Fun.protect ~finally:(fun () ->
      Mirage_crypto_rng_mkernel.kill rng;
      Mnet.kill daemon)
  @@ fun () -> Mehari_mirage.run ?port ~certs tcp (Logger.logger router)

open Cmdliner

let ipv4 =
  let doc = "The IP address of the unikernel." in
  let ipaddr = Arg.conv (Ipaddr.V4.Prefix.of_string, Ipaddr.V4.Prefix.pp) in
  let open Arg in
  required & opt (some ipaddr) None & info [ "ipv4" ] ~doc ~docv:"IPv4"

let ipv4_gateway =
  let doc = "The IP gateway." in
  let ipaddr = Arg.conv (Ipaddr.V4.of_string, Ipaddr.V4.pp) in
  let open Arg in
  value & opt (some ipaddr) None & info [ "ipv4-gateway" ] ~doc ~docv:"IPv4"

let port =
  let doc = "The Gemini port" in
  let open Arg in
  value & opt (some int) None & info [ "p"; "port" ] ~doc ~docv:"PORT"

let term =
  let open Term in
  const run $ ipv4 $ ipv4_gateway $ port

let cmd =
  let info = Cmd.info "mehari" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
