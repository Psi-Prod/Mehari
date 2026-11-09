# Mirage OS unikernel example


Setup dev environment:
```bash
$ opam install mehari-mirage solo5 ocaml-solo5 unic mfetch
```

Vendor dependencies:
```bash
$ cd unikernel/
$ unic infer -r . -x _build -x vendors -o _mfetch
$ mfetch --with-dune-file
```

Setup network bridge:
```bash
$ ip link add name service type bridge
$ ip addr add 10.0.0.1/24 dev service
$ ip tuntap add name tap0 mode tap
$ ip link set tap0 master service
$ ip link set service up
$ ip link set tap0 up
```

Generate a self signed certchains using `openssl`:
```bash
$ openssl req -x509 -newkey rsa:2048 -nodes -days 825 \
  -keyout pk.pem -out certs.pem -subj "/CN=localhost" \
  -addext "basicConstraints=CA:FALSE" \
  -addext "keyUsage=critical,digitalSignature,keyEncipherment" \
  -addext "extendedKeyUsage=serverAuth" \
  -addext "subjectAltName=DNS:localhost,IP:10.0.0.2"
```

Setup an image containing our certchain (you can use tools such as `mkcert -key-file pk.pem -cert-file certs.pem 10.0.0.2 localhost` to generate a local signed certificate):
```bash
$ mfat make -s 2048 certs.img
$ mfat mkdir certs.img localhost
$ mfat write certs.img localhost/key.pem pk.pem
$ mfat write certs.img localhost/cert.pem certs.pem
```

Compile and run:
```bash
$ dune build --profile=release
$ solo5-hvt --net:service=tap0 --block:certs=certs.img -- ./_build/solo5/unikernel/unikernel.exe --solo5:quiet --ipv4=10.0.0.2/8 --logging-levels "mehari.log:info"
```

Request the URL `gemini://10.0.0.2:1965/` with your Gemini client.
