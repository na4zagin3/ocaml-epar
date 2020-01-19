(lang dune 1.11)
(name epar)
(source (github na4zagin3/ocaml-epar))
(license MIT)
(authors "SAKAMOTO Noriaki <mrty.ityt.pt@gmail.com>")
(maintainers "SAKAMOTO Noriaki <mrty.ityt.pt@gmail.com>")

(package
 (name epar)
 (synopsis "OCaml implementation of Extensible Plain-text ARchive (EPAR) format")
 (depends
  (dune (> 1.5))
  (yaml (>= 2.0.0))
  (ppx_jane (< "v0.14"))))
