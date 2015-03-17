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

rm -f rubygrammar.c rubygrammar.h
./byacc/yacc -d -g -o rubygrammar.c -P -t -v grammar.y
if test $? -ne 0; then
  echo "byacc failed"
  exit 1
fi
chmod -w rubygrammar.h
chmod -w rubygrammar.c
# no longer copying to server src directory
echo "byacc ok"

# for zenspider to get grammatical structure for comparison to MRI
if [ -d compare ]; then
    (cd compare && rake && cp diff*.diff ..)

    GEMSTONE=~/Work/git/GemStone-30866.Darwin-i386

    cc -fmessage-length=0 -Wformat -Wtrigraphs -Wcomment -Wsystem-headers -Wtrigraphs -Wno-aggregate-return -Wswitch -Wshadow -Wunused-value -Wunused-variable -Wunused-label -Wno-unused-function -Wchar-subscripts -Wmissing-braces -Wmultichar -Wparentheses -Wsign-compare -Wsign-promo -Wwrite-strings -Wreturn-type -g -DFLG_DEBUG=1 -m64  -pipe -D_REENTRANT -DNOT_JAVA_VM -D_GNU_SOURCE -pthread  -fPIC -fno-strict-aliasing -fno-exceptions -I. -I$GEMSTONE/include -x c++ -c rubygrammar.c -o rubygrammar.o
fi
