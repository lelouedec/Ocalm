
all:
	ocamlbuild main.byte
	mv main.byte mincamlc

unit-test:
	ocamlbuild tests/alpha_test.byte
	./alpha_test.byte

clean:
	rm -rf _build

cleanest: clean
	rm -f min-ml
