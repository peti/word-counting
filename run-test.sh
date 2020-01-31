#! /usr/bin/env bash

set -eu -o pipefail

tests=$( sed -n -e "s/^executable //p" word-counting.cabal )
testData=/dev/shm/word-counting-data.txt

cabal install --install-method=symlink --installdir=$PWD/bin

if [ ! -f $testData ]; then
    echo "generate random test data ..."
    dd if=/dev/urandom of="$testData" bs=1G count=1 iflag=fullblock status=progress
fi

benchCmd=""
for exe in $tests; do
    benchCmd+=" 'bin/$exe <$testData'"
done
eval bench --output result.html $benchCmd
