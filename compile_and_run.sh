#! /bin/sh

MINCAMLC=ocaml/mincamlc
EBI=ARM/`basename $1`

echo "--------------------"
echo "Compiling $1..."
$MINCAMLC -asml $1 >> $EBI.s

echo "Compiling ARM..."
cd ARM/ && (make 2> /dev/null 1> /dev/null) && cd .. 

echo "Running program $1..." 
$EBI.arm

