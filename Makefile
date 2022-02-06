
coq-of-ocaml/Set.v: ocaml/set.ml
	coq-of-ocaml -output $@ $<

all: coq-of-ocaml
