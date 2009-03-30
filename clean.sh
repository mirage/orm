#!/bin/sh

set -ex

make clean

(cd tests && make clean)
(cd tests && make -f Makefile.test clean)
