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
 (synopsis "A Mirage friendly Gemini server")
 (depends
  (dune
   (>= 3.0))
  (magic-mime
   (>= 1.3.0))
  (lwt
   (>= 5.6.0))
  (lwt_ppx
   (>= 2.1.0))
  (ocaml
   (>= 4.13))))
