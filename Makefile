.PHONY: clean headers main

main:
	@dune build bin/fiordtop.exe bin/fiordcheck.exe
	@ln -f -s _build/default/bin/fiordtop.exe fiordtop.exe
	@ln -f -s _build/default/bin/fiordcheck.exe fiordcheck.exe

clean:
	@dune clean
	@rm -f fiordtop.exe fiordcheck.exe
	@rm -f {lib,bin}/.merlin

headers:
	@headache -h HEADER lib/*.ml lib/*.mli bin/*.ml bin/*.mll bin/*.mly
