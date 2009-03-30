#!/bin/sh

set -ex

make -f Makefile.ormgen
make -f Makefile.test
./ormtest -verbose
mv ormtest_debug.ml ormtest.ml && touch ormtest.ml
mv ormtest_debug.mli ormtest.mli && touch ormtest.mli
make -Bf Makefile.test
./ormtest -verbose
