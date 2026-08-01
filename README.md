# Mehari

Mehari is a library for building Gemini servers. It fully
implements the
[Gemini protocol specification](https://geminiprotocol.net/docs/protocol-specification.gmi).

It takes inspiration from [Dream](https://github.com/aantron/dream), a
tidy feature-complete Web framework, and from [Nightmare](https://github.com/funkywork/nightmare), a powerful typesafe layer compatible with Dream.

<p align="center">
  <img src="https://user-images.githubusercontent.com/59396366/211079934-44f65ed1-8cf7-4193-a815-8da94a85be5d.png" alt="banner"/>
</p>

Mehari provides several packages. See [here](https://docs.heyplzlookat.me/mehari/index.html#interface).
Many [examples](https://github.com/Psi-Prod/Mehari/tree/master/examples) are also provided.

## Installation

```
opam install mehari
```

## Features

- Mirage OS friendly
- Type safe route API
- Static files serving
- MIME type inference from file content (using experimental [Conan](https://github.com/mirage/conan/) support)
- Rate limit
- Virtual hosting using TLS SNI
- CGI support

## Important links

- API documentation: https://docs.heyplzlookat.me/mehari/
- Tutorial: https://docs.heyplzlookat.me/mehari/index.html#tutorial
- Issues: https://github.com/Psi-Prod/Mehari/issues


## Building the unikernel

Setup dev environment:
```bash
$ opam install mehari-mirage solo5 ocaml-solo5
$ opam install ca-certs-nss dns dns-client mfat
$ opam pin zarith.dev git+https://github.com/mirage/Zarith.git#df8969d9bb95cffca13e31a615189ab27ada0684
```

Vendor dependencies:
```bash
$ ./unikernel/vendor.sh
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

Create `dune` and `dune-workspace` files at the root of the repo:

``` bash
$ cat >dune<<EOF
(vendored_dirs vendors)
EOF
$ cat >dune-workspace<<EOF
(lang dune 3.23)
(context (default))
(context (default
 (name solo5)
 (host default)
 (toolchain solo5)
 (disable_dynamically_linked_foreign_archives true)))
EOF
```


```bash
$ mfat make -s 2048 certs.img
$ mfat write certs.img key.pem key.pem
$ mfat write certs.img certs.pem cert.pem
```

Compile and run the unikernel:

```bash
$ dune build --profile=release
$ solo5-hvt --net:service=tap0 --block:certs=certs.img -- ./_build/solo5/unikernel/unikernel.exe --solo5:quiet --ipv4=10.0.0.2/8
```

## License

Distributed under the **LGPL-3.0 License**. See [license](LICENSE) for more information.
