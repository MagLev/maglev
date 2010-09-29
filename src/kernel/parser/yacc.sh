#! /usr/bin/bash

./byacc/yacc -d -g -o rubygrammar.c -P -t -v grammar.y
if [ $? -ne 0 ]; then
  echo "byacc failed"
  exit 1
fi

fileList='rubylex_tab.hc rubyast.c rubyast.hf rubyast.ht
rubygrammar.c rubygrammar.h rubyparser.h'

srcDir=../../../../svn/src

for each in $fileList
do
  rm $srcDir/$each
  cat generated.txt $each > $srcDir/$each
done

echo "byacc ok"
