#!/bin/sh

grep -E -h -v '^(#.*)?$' $@ |  sed -E 's/(.*)=.*/\1/; s/^(un)?set/reset/g; /macro/d'
