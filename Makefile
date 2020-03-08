.PHONY: clean headers main

main:
	@dune build main.exe
	@ln -f -s _build/default/main.exe main

clean:
	@dune clean
	@rm -f main
	@rm -f .merlin

headers:
	@headache -h HEADER *.ml *.mli *.mll *.mly
