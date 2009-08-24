#!/bin/sh

make -f Makefile.camlp4 clean all
make -f Makefile.sqlaccess clean all
cd lib_test && make all
