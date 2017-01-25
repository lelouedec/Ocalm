#! /bin/sh

echo "hello"


nb_cases=0
nb_failures=0

echo "--------------------"
echo "testing Code generation"
cd ../tests/gen-code/valid
mkdir Actual
for test_case in *.ml
do
    nb_cases=$((nb_cases + 1))
    echo "testing Code generation on: $test_case"
    #compiling
    #./a > Actual/"$test_case".actual
    #./a > Expected/"$test_case".expected
     if (cmp -s Actual/"$test_case".actual Expected/"$test_case".expected)
     then
     	echo "OK "
     else 
     	nb_failures=$((nb_failures + 1))
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


