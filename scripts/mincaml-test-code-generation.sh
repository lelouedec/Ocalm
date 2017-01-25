#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

MINCAMLC=ocaml/mincamlc
EBI=ARM
nb_cases=0
nb_failures=0

echo "--------------------"
echo "testing scripts against asml tool"
for test_case in tests/asml/*.ml
do
    nb_cases=$((nb_cases + 1))
    echo "generating files for testing: $test_case"

    $MINCAMLC -asml "$test_case" 2> /dev/null 1> /dev/null
    $MINCAMLC "$test_case" 2> /dev/null 1> /dev/null

    test_str="$test_case"
    test_out=${test_str%.ml}.asml
    exec_out=${test_str%.ml}.s

    mv $exec_out $EBI/`basename $test_case`
    make -C $EBI 2> /dev/null 1> /dev/null

    echo "testing generation and ground truth"
    mincaml=$($EBI/`basename $test_case`.arm)
    groundt=$($ASML "$test_out")
    if [ "$(<$exec_out)" = "$groundt" ];
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
