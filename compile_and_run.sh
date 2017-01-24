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
mincaml=$($EBI.arm)

groundt=$(ocaml $1)
if [ "$mincaml" = "$groundt" ]; then
  echo "Program correct"
else
  echo "Program incorrect" 
fi
