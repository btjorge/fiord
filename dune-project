(lang dune 1.11)
(using menhir 2.0)

(generate_opam_files)

(name fiord)
(license Apache-2.0)
(source
 (github btjorge/fiord))
(authors
 "Benjamin Terra-Jorge <benjamin.terra-jorge@laposte.net>")
(maintainers
 "Benjamin Terra-Jorge <benjamin.terra-jorge@laposte.net>")

(package
 (name fiord)
 (synopsis "First order logic theorem prover")
 (depends menhir))
