#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

MINCAMLC=ocaml/mincamlc
ASML=tools/asml
EBI=ARM
nb_cases=0
nb_failures=0

echo "--------------------"
echo "testing valid scripts"
for test_case in tests/gen-code/valid/*.ml
do
    nb_cases=$((nb_cases + 1))
    echo "generating files for testing: $test_case"

    $MINCAMLC -asml "$test_case" 2> /dev/null 1> /dev/null
    $MINCAMLC "$test_case" 2> /dev/null 1> /dev/null

    test_str="$test_case"
    test_out=${test_str%.ml}.asml
    exec_out=${test_str%.ml}.s

    mv $exec_out $EBI/`basename $exec_out`
    make -C $EBI 2> /dev/null 1> /dev/null
    
    mincaml=$($EBI/`basename ${test_str%.ml}.arm`)
    asmltoo=$($ASML "$test_out")
    groundt=$(ocaml $test_case)
    echo "ASML tool :" "$asmltoo"", OCaml compiler :" "$groundt"
    if [ "$mincaml" = "$asmltoo" && "$mincaml" = "$groundt" ];
    then
        echo "OK"
    else
        nb_failures=$((nb_failures + 1))
        echo "KO ~~~~~~~~~~ OK expected!!!"
    fi
done

echo "run $nb_cases tests"
if test $nb_failures -gt $((0))
then
    echo "$nb_failures/$nb_cases tests failed!"
else
    echo "all passed!"
fi
