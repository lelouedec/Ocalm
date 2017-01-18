#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

MINCAMLC=ocaml/mincamlc
nb_cases=0
nb_failures=0

echo "--------------------"
echo "testing valid scripts"
for test_case in tests/typechecking/valid/*.ml
do
    nb_cases=$((nb_cases + 1))
    echo "testing type checking on: $test_case"
    if $MINCAMLC -t "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK"
    else
        nb_failures=$((nb_failures + 1))
        echo "KO ~~~~~~~~~~ OK expected!!!"
    fi
done

echo "--------------------"
echo "testing invalid scripts"
for test_case in tests/typechecking/invalid/*.ml
do
    nb_cases=$((nb_cases + 1))
    echo "testing type checking on: $test_case"
    if $MINCAMLC -t "$test_case" 2> /dev/null 1> /dev/null
    then
        nb_failures=$((nb_failures + 1))
        echo "OK ~~~~~~~~~~ KO expected!!!"
    else 
        echo "KO"
    fi
done

echo "run $nb_cases tests"
if test $nb_failures -gt $((0))
then
    echo "$nb_failures/$nb_cases tests failed!"
else
    echo "all passed!"
fi
