
othello: othello_main.ml
	ocamlc -o othello othello_main.ml

clean:
	rm -f othello *~ *.cmo *.cmi
