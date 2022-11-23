(lang dune 2.9)

(version 0.1)

(name mehari)

(license "GPL-3.0")

(authors "The mehari programmers")

(maintainers "tim.arnouts@protonmail.com" "lelolartichaut@laposte.net")

(source
 (github Psi-Prod/Mehari))

(homepage "https://github.com/Psi-Prod/Mehari")

(generate_opam_files true)

(package
 (name mehari)
 (synopsis "A Mirage OS friendly library for building Gemini servers")
 (description
  "Mehari is a Mirage OS friendly library for building Gemini servers.\nIt fully implements the Gemini protocol specification and aims to expose a clean and simple API.\nIt takes heavy inspiration from Dream, a tidy, feature-complete Web framework.")
 (depends
  (dune
   (>= 3.0))
  (ocaml
   (>= 4.13))
  (magic-mime
   (>= 1.3.0))
  (lwt
   (>= 5.6.0))
  (lwt_ppx
   (>= 2.1.0))
  (re
   (>= 1.10.4))
  (tls
   (>= 0.15.3))
  (uri
   (>= 4.2.0))))
