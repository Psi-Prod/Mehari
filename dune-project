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
   (>= 4.14))
  (conan-database
   (>= 0.0.2))
  (conan
   (>= 0.0.2))
  (magic-mime
   (>= 1.3.0))
  (mirage-clock
   (>= 4.2.0))
  (mirage-channel
   (>= 4.1.0))
  (lwt
   (>= 5.6.1))
  (lwt_ppx
   (>= 2.1.0))
  (re
   (>= 1.10.4))
  (tcpip
   (>= 7.1.2))
  (tls
   (>= 0.15.3))
  (tls-mirage
   (>= 0.15.4))
  (uri
   (>= 4.2.0))))

(package
 (name mehari_lwt_unix)
 (synopsis "Mehari IO implementation using Lwt")
 (depends
  mehari
  (lwt
   (>= 5.6.1))
  (mirage-clock-unix
   (>= 4.2.0))
  (mirage-flow-unix
   (>= 3.0.0))
  (tcpip
   (>= 7.1.2))))

(package
 (name mehari_eio)
 (synopsis "Mehari IO implementation using EIO")
 (depends
  (ocaml
   (>= 5.0.0))
  eio
  mehari))
