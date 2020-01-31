#! /usr/bin/env bash

tests=$( sed -n -e "s/^executable //p" word-counting.cabal )
testData=/dev/shm/word-counting-data.txt

cabal build

if [ ! -f $testData ]; then
    echo "generate random test data ..."
    dd if=/dev/urandom of="$testData" bs=1G count=1 iflag=fullblock status=progress
fi

for exe in $tests; do
    echo "$exe"
    exe=$(cabal-plan list-bin "exe:$exe")
    "$exe" <"$testData"
done
