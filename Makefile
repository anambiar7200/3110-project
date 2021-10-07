.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f rummikub.zip
	zip -r rummikub.zip . -x@exclude.lst

clean:
	dune clean
	rm -f rummikub.zip

doc:
	dune build @doc
