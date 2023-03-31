# Mehari

Mehari is a cross-platform library for building Gemini servers. It fully
implements the
[Gemini protocol specification](https://gemini.circumlunar.space/docs/specification.gmi).

It takes heavy inspiration from [Dream](https://github.com/aantron/dream), a
tidy, feature-complete Web framework.

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
- Static files serving
- MIME type inference from file content (using experimental [Conan](https://github.com/mirage/conan/) support)
- Rate limit
- Virtual hosting using SNI
- CGI
- Long-running TCP connection

## Feedback

Since Mehari is in its early stages, any feedback and contributions regarding the API or security are appreciated.

## Important links

- API documentation: https://docs.heyplzlookat.me/mehari/
- Tutorial: https://docs.heyplzlookat.me/mehari/index.html#tutorial
- Issues: https://github.com/Psi-Prod/Mehari/issues

## License

Distributed under the **LGPL-3.0 License**. See [license](LICENSE) for more information.
