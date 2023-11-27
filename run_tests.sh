#!/bin/bash

errors=0
echo "----------------------"
echo "GOOD TESTS"
echo "----------------------"
cd tests/good
for i in *.lat; do
    echo ""
    echo "TESTING $i"
    echo ""
    if ../../latc_llvm $i; then
        errors=$errors #noop
    else
        errors=$(($errors + 1))
    fi
done

echo "----------------------"
echo "BAD TESTS"
echo "----------------------"
cd ../bad
for i in *.lat; do
    echo ""
    echo "TESTING $i"
    echo ""
    if ../../latc_llvm $i; then
        errors=$(($errors + 1))
    fi
done

if [ "$errors" -eq 0 ]; then
    echo "Passed all tests!"
else
    echo "$errors tests failed!"
fi