(lang dune 2.6)
(name httpaf_caged)
(version 1.0.0)

(generate_opam_files true)

(source (github thedufer/httpaf_caged))
(license BSD-3-clause)
(authors "Aaron Dufour")
(maintainers "thedufer@gmail.com")

(package
 (name httpaf_caged)
 (synopsis "A higher-level httpaf server interface")
 (description "Exposes a standard request->response server interface, as well as helpers for parsing various standard headers (cookies, accept, etc).")
 (depends
  (httpaf (>= 0.6.6))
  (angstrom (>= 0.14.1))
  (dune (>= 2.6))
  (uri (>= 3.1.0))
  ))