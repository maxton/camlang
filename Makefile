
camlang: camlang.ml
	ocamllex lexer.mll
	menhir parser.mly
	ocamlc -c sexp.mli
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c sexp.ml
	ocamlc -c camlang.ml
	ocamlc -g -o camlang sexp.ml lexer.cmo parser.cmo camlang.cmo

clean:
	rm -f *.cmo *.cmi camlang
