#!/bin/sh

set -ex

make clean
make -f Makefile.ormgen clean
make -f Makefile.test clean
