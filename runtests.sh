#!/bin/sh

set -ex

make -f Makefile.ormgen
make -f Makefile.test
./ormtest -verbose
mv ormtest_debug.ml ormtest.ml
mv ormtest_debug.mli ormtest.mli
make -f Makefile.test
./ormtest -verbose
