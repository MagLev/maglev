#!/bin/bash

PARSERDIR=$(cd $(dirname $0) ; pwd)
cd $PARSERDIR

if ! test -x ./byacc/yacc; then
  cd byacc
  ./configure
  if [ -n "$(which gmake)" ]; then
    gmake
  else
    make
  fi
  cd ..
  if test $? -ne 0; then
    echo "compiling byacc failed, please compile it in ${PARSERDIR}/byacc"
    exit 1
  fi
fi

rm -f rubygrammar.c
./byacc/yacc -d -g -o rubygrammar.c -P -t -v grammar.y
if test $? -ne 0; then
  echo "byacc failed"
  exit 1
fi
chmod -w rubygrammar.h
chmod -w rubygrammar.c
# no longer copying to server src directory
echo "byacc ok"
