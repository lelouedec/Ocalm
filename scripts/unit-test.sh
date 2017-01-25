#! /bin/sh

cd ../ocaml
ocamlfind ocamlc -o test -package oUnit -linkpkg -g id.ml  asml.ml register_alloc.ml  unit_test.ml
./test >  ../tests/unit-test-result

