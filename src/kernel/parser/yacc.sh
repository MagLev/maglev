#!/bin/bash

# ANY error should stop the script and exit non-zero
# This is why we need to switch to makefiles
set -e
# set -xv

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

rm -f rubygrammar.c rubygrammar.h
./byacc/yacc -d -g -o rubygrammar.c -P -t -v grammar.y
if test $? -ne 0; then
  echo "byacc failed"
  exit 1
fi

# for zenspider to get grammatical structure for comparison to MRI
if [ -d compare ]; then
    (cd compare && rake && cp diff*.diff ..)
fi
