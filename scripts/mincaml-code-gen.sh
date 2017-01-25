#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

MINCAMLC=ocaml/mincamlc





nb_cases=0
nb_failures=0

echo "--------------------"
echo "testing valid scripts"
for test_case in tests/gen-code/valid/*.ml
do
    nb_cases=$((nb_cases + 1))
    echo "testing code-gen on: $test_case"
     b=$(basename $test_case)
    EBI=../ARM/$b
    $MINCAMLC "$test_case" > $b.s
    echo "Compiling ARM..."
	make -C ARM/

	echo "Running program $1..." 
	mincaml=$($EBI.arm)

	groundt=$(ocaml $1)
    if [ "$mincaml" = "$groundt" ];
    then
        echo "OK"
    else
        nb_failures=$((nb_cases + 1))
        echo "KO ~~~~~~~~~~ OK expected!!!"
    fi
done