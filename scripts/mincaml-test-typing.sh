#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

MINCAMLC=ocaml/mincamlc

echo "--------------------"
echo "testing valid scripts"
for test_case in tests/typechecking/valid/*.ml
do
    echo "testing type checking on: $test_case"
    if $MINCAMLC -t "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK"
    else 
        echo "KO ~~~~~~~~~~ OK expected!!!"
    fi
done

echo "--------------------"
echo "testing invalid scripts"
for test_case in tests/typechecking/invalid/*.ml
do
    echo "testing type checking on: $test_case"
    if $MINCAMLC -t "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK ~~~~~~~~~~ KO expected!!!"
    else 
        echo "KO"
    fi
done
