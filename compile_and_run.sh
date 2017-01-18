#! /bin/sh

MINCAMLC=ocaml/mincamlc
EBI=./ARM/`basename $1`

cd ./ocaml
make
cd ..

echo "Compiling $1..."
$MINCAMLC -asml $1 > $EBI.s

echo "Compiling ARM..."
make -C ARM/

echo "Running program $1..." 
$EBI.arm

