(lang dune 3.0)

(version 0.4)

(name mehari)

(license "LGPL-3.0-or-later")

(authors "The mehari programmers")

(maintainers "tim.arnouts@protonmail.com" "lelolartichaut@laposte.net")

(source
 (github Psi-Prod/Mehari))

(homepage "https://github.com/Psi-Prod/Mehari")

(generate_opam_files true)

(package
 (name mehari)
 (synopsis "A cross-platform library for building Gemini servers")
 (description
  "Mehari is a cross-platform library for building Gemini servers.\nIt fully implements the Gemini protocol specification.\nIt takes heavy inspiration from Dream, a tidy, feature-complete Web framework.")
 (depends
  (ocaml
   (>= 4.14))
  (conan
   (>= 0.0.5))
  (conan-database
   (>= 0.0.5))
  (logs
   (>= 0.7.0))
  (re
   (>= 1.10.4))
  (tls
   (>= 0.16.0))
  (uri
   (>= 4.2.0))
  (x509
   (>= 0.16.2))))

(package
 (name mehari-mirage)
 (synopsis "Mehari IO implementation for MirageOS")
 (depends
  (mehari
   (= :version))
  (ipaddr
   (>= 5.3.1))
  (lwt
   (>= 5.6.1))
  (mirage-channel
   (>= 4.1.0))
  (mirage-time
   (>= 3.0.0))
  (tcpip
   (>= 7.1.2))
  (tls-mirage
   (>= 0.15.4))
  (tls-lwt
   (>= 0.16.0))))

(package
 (name mehari-lwt-unix)
 (synopsis "Mehari IO implementation using Lwt and Unix bindings")
 (depends
  (mehari
   (= :version))
  (mehari-mirage
   (= :version))
  (lwt
   (>= 5.6.1))
  (mirage-clock-unix
   (>= 4.2.0))
  (mirage-flow-unix
   (>= 3.0.0))
  (mirage-time-unix
   (>= 3.0.0))
  (tcpip
   (>= 7.1.2))))

(package
 (name mehari-eio)
 (synopsis "Mehari IO implementation using Eio")
 (depends
  (ocaml
   (>= 5.0.0))
  (mehari
   (= :version))
  (eio
   (>= 1.0))
  (logs
   (>= 0.7.0))
  (tls
   (>= 0.15.4))
  (tls-eio
   (>= 0.15.5))))
