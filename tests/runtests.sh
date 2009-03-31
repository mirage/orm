#!/bin/sh

set -ex

make clean
make
mv ormtest_debug.ml ormtest.ml
mv ormtest_debug.mli ormtest.mli
make -Bf Makefile.test
./ormtest -verbose
mv ormtest_normal.ml ormtest.ml
mv ormtest_normal.mli ormtest.mli
make -Bf Makefile.test
./ormtest -verbose
