#!/bin/sh

sudo make uninstall && make && sudo make install && (cd tests && ./runtests.sh)
