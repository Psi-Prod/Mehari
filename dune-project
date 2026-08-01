(lang dune 3.21)

(version 0.5)

(name mehari)

(license "LGPL-3.0-or-later")

(authors "The mehari programmers")

(maintainers "tim.arnouts@protonmail.com" "lelolartichaut@laposte.net")

(source
 (github Psi-Prod/Mehari))

(homepage "https://github.com/Psi-Prod/Mehari")

(maintenance_intent "(latest)")

(generate_opam_files)

(package
 (name mehari)
 (synopsis "A library for building Gemini servers.")
 (description
  "Mehari is a library for building Gemini servers.\nIt fully implements the Gemini protocol specification.\nIt takes heavy inspiration from Dream, a tidy, feature-complete Web framework.")
 (depends
  (ocaml
   (>= 5.4))
  (conan
   (>= 0.0.7))
  (conan-database
   (>= 0.0.7))
  (ipaddr
   (>= 5.6.2))
  (logs
   (>= 0.10.0))
  (mirage-ptime
   (>= 5.2.0))
  (tls
   (>= 2.1.2))
  (uri
   (>= 4.4.0))
  (alcotest :with-test)
  (utop :with-dev-setup)
  (ocamlformat :with-dev-setup)
  (ocp-indent :with-dev-setup)
  (merlin :with-dev-setup)
  (ocaml-lsp-server :with-dev-setup)))

(package
 (name mehari-miou)
 (synopsis "Mehari server implementation using Miou.")
 (depends
  (mehari
   (= :version))
  (tls-miou-unix
   (>= 2.1.2))))

(package
 (name mehari-mirage)
 (synopsis "Mehari server implementation for Mirage.")
 (depends
  (mehari
   (= :version))
  (mkernel
   (>= 0.0.3))
  (mnet
   (>= 0.0.4))
  (mnet-tls
   (>= 0.0.4))))
