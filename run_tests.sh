#!/bin/bash

echo "GOOD TESTS"
cd tests/good
for i in *.lat; do
    echo "TESTING $i"
    ../../latc_llvm $i
done

echo "BAD TESTS"
cd ../bad
for i in *.lat; do
    echo "TESTING $i"
    ../../latc_llvm $i
done

echo "DONE"