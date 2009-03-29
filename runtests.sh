#!/bin/sh

set -ex

make -f Makefile.ormgen
make -f Makefile.test
./ormtest -verbose
