tests := $(wildcard tests/*.ml)
tests_byte := $(wildcard *_test.byte)


all: 
	ocamlbuild main.byte 
	mv main.byte mincamlc


unit-test:
	ocamlbuild $(patsubst %.ml,%.byte,$(tests))
	for test in $(tests_byte); do ./$$test; done

clean:
	rm -rf _build

cleanest: clean
	rm -f min-ml
