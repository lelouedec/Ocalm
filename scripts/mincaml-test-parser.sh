#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

# TODO change this to point to your mincamlc executable if it's different, or add
# it to your PATH. Use the appropriate option to run the parser as soon
# as it is implemented
MINCAMLC=ocaml/mincamlc

# run all test cases in syntax/valid and make sure they are parsed without error
# run all test cases in syntax/invalid and make sure the parser returns an error

# TODO extends this script to run test in subdirectories
# 

nb_cases=0
nb_failures=0

echo "--------------------"
echo "testing valid scripts"
for test_case in tests/syntax/valid/*.ml
do
    nb_cases=$((nb_cases + 1))
    echo "testing parser on: $test_case"
    if $MINCAMLC -p "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "OK"
    else
        nb_failures=$((nb_cases + 1))
        echo "KO ~~~~~~~~~~ OK expected!!!"
    fi
done

echo "--------------------"
echo "testing invalid scripts"
for test_case in tests/syntax/invalid/*.ml
do
    nb_cases=$((nb_cases + 1))
    echo "testing parser on: $test_case"
    if $MINCAMLC -p "$test_case" 2> /dev/null 1> /dev/null
    then
        nb_failures=$((nb_failures + 1))
        echo "OK ~~~~~~~~~~ KO expected!!!"
    else
        echo "KO"
    fi
done

rm tests/typechecking/valid/*.{asml,s} 2> /dev/null
rm tests/typechecking/invalid/*.{asml,s} 2> /dev/null

echo "run $nb_cases tests"
if test $nb_failures -gt $((0))
then
    echo "$nb_failures/$nb_cases tests failed!"
else
    echo "all passed!"
fi
