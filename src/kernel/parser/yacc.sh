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

./byacc/yacc -d -g -o rubygrammar.c -P -t -v grammar.y
if test $? -ne 0; then
  echo "byacc failed"
  exit 1
fi

if test -x ../../../../copyChangedParserFilesToServer.sh ; then
  cd ../../../../
  ./copyChangedParserFilesToServer.sh -q
else
  fileList='rubylex_tab.hc rubyast.c rubyast.hf rubyast.ht
  rubygrammar.c rubygrammar.h rubyparser.h'

  srcDir=../../../../svn/src

  for each in $fileList
  do
    rm -f $srcDir/$each
    cat generated.txt $each > $srcDir/$each
    chmod -w $srcDir/$each
  done
  echo "byacc ok"
fi
