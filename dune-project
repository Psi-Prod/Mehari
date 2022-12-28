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
  (logs
   (>= 0.7.0))
  (magic-mime
   (>= 1.3.0))
  (mirage-clock
   (>= 4.2.0))
  (tls
   (>= 0.15.4))
  (uri
   (>= 4.2.0))
  (x509
   (>= 0.16.2))))

(package
 (name mehari_mirage)
 (synopsis "Mehari IO implementation for MirageOS")
 (depends
  mehari
  (ipaddr
   (>= 5.3.1))
  (lwt
   (>= 5.6.1))
  (lwt_ppx
   (>= 2.1.0))
  (mirage-channel
   (>= 4.1.0))
  (mirage-time
   (>= 3.0.0))
  (re
   (>= 1.10.4))
  (tcpip
   (>= 7.1.2))
  (tls-mirage
   (>= 0.15.4))))

(package
 (name mehari_lwt_unix)
 (synopsis "Mehari IO implementation using Lwt")
 (depends
  mehari
  mehari_mirage
  (lwt
   (>= 5.6.1))
  (lwt_ppx
   (>= 2.1.0))
  (mirage-clock-unix
   (>= 4.2.0))
  (mirage-flow-unix
   (>= 3.0.0))
  (mirage-time-unix
   (>= 3.0.0))
  (tcpip
   (>= 7.1.2))))

(package
 (name mehari_eio)
 (synopsis "Mehari IO implementation using EIO")
 (depends
  (ocaml
   (>= 5.0.0))
  mehari
  (eio
   (>= 0.6))
  (mirage-clock-unix
   (>= 4.2.0))
  (logs
   (>= 0.7.0))
  (tls
   (>= 0.15.4))
  (tls-eio
   (>= 0.15.5))
  (uri
   (>= 4.2.0))))
