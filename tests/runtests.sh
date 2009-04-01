#!/bin/sh

set -ex

SCHEMA="ormtest foreign"
make clean
make
for i in ${SCHEMA}; do
  mv ${i}_debug.ml ${i}.ml
  mv ${i}_debug.mli ${i}.mli
done
make -Bf Makefile.test
./ormtest -verbose
for i in ${SCHEMA}; do
  mv ${i}_normal.ml ${i}.ml
  mv ${i}_normal.mli ${i}.mli
done
make -Bf Makefile.test
./ormtest -verbose
