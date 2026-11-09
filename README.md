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

- Type safe route API
- Static files serving
- MIME type inference from file content (using experimental [Conan](https://github.com/mirage/conan/) support)
- Rate limiting
- Virtual hosting using TLS SNI
- CGI scripts support
- Mirage OS friendly (see [the dedicated example](unikernel/README.md))

## Important links

- API documentation: https://docs.heyplzlookat.me/mehari/
- Tutorial: https://docs.heyplzlookat.me/mehari/index.html#tutorial
- Issues: https://github.com/Psi-Prod/Mehari/issues

## License

Distributed under the **LGPL-3.0 License**. See [license](LICENSE) for more information.
