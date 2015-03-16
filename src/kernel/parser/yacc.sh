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
bison -r all grammar.y && mv grammar.output maglev.output && rm grammar.tab.c
~/Work/p4/zss/src/ruby_parser/dev/yack.rb maglev.output > maglev.txt
diff -u ~/Work/p4/zss/src/ruby_parser/dev/yacc18.txt maglev.txt > diff18.diff
diff -u ~/Work/p4/zss/src/ruby_parser/dev/yacc19.txt maglev.txt > diff19.diff
