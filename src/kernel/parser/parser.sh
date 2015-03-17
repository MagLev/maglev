#!/bin/bash

PARSERDIR=$(cd $(dirname $0) ; pwd)
MAGLEV_HOME=$(cd $(dirname $0)/../../.. ; pwd)
GEMSTONE=$MAGLEV_HOME/gemstone

echo "Building the yacc executable"

cd $PARSERDIR
if ! test -x ./byacc/yacc; then
  cd byacc
  ./configure
  if [ -n "$(which gmake)" ]; then
    gmake
  else
    make
  fi
  cd -
  if test $? -ne 0; then
    echo "compiling byacc failed, please compile it in ${PARSERDIR}/byacc"
    exit 1
  fi
fi

chmod +w rubygrammar.{c,dot,h,output}
#rm -f rubygrammar.c

./byacc/yacc -d -g -o rubygrammar.c -P -t -v grammar.y

if test $? -ne 0; then
  echo "  byacc failed"
  exit 1
fi
chmod -w rubygrammar.{h,c}
echo "  byacc ok"

echo "Compiling rubygrammar.o"
/usr/bin/g++ -fmessage-length=0 -fcheck-new \
  -Wformat -Wtrigraphs -Wcomment -Wsystem-headers -Wtrigraphs -Wno-aggregate-return \
  -Wswitch -Wshadow -Wunused-value -Wunused-variable -Wunused-label -Wno-unused-function \
  -Wchar-subscripts -Wmissing-braces -Wmultichar -Wparentheses -Wsign-compare -Wsign-promo \
  -Wwrite-strings -Wreturn-type -Wuninitialized -Werror  \
  -g -O3 -DFLG_FAST=1 \
  -m64  -pipe -D_REENTRANT -DNOT_JAVA_VM -D_GNU_SOURCE -pthread  -fPIC \
  -fno-strict-aliasing -fno-exceptions \
  -I. -I$GEMSTONE/include -x c++ \
  -c rubygrammar.c -o rubygrammar.o

if test $? -ne 0; then
  echo "compiling rubygrammar.o failed"
  exit 1
fi

echo "Compiling rubyast.o"
/usr/bin/g++ -fmessage-length=0 -fcheck-new \
  -Wformat -Wtrigraphs -Wcomment -Wsystem-headers -Wtrigraphs -Wno-aggregate-return \
  -Wswitch -Wshadow -Wunused-value -Wunused-variable -Wunused-label -Wno-unused-function \
  -Wchar-subscripts -Wmissing-braces -Wmultichar -Wparentheses -Wsign-compare -Wsign-promo \
  -Wwrite-strings -Wreturn-type -Wuninitialized -Werror  \
  -g -O3 -DFLG_FAST=1 \
  -m64  -pipe -D_REENTRANT -DNOT_JAVA_VM -D_GNU_SOURCE -pthread  -fPIC \
  -fno-strict-aliasing -fno-exceptions \
  -I. -I$GEMSTONE/include -x c++ \
  -c rubyast.c -o rubyast.o

if test $? -ne 0; then
  echo "compiling rubyast.o failed"
  exit 1
fi

rm -f libmagparse.so

echo "Linking libmagparse.so"
/usr/bin/g++ -shared -Wl,-Bdynamic,-hlibmagparse.so  -Wl,--version-script=magparse.exp \
  rubyast.o rubygrammar.o  \
  -m64 -lpthread -lcrypt -ldl -lc -lm -lrt -o libmagparse.so \
  -Wl,--warn-unresolved-symbols

chmod 555 libmagparse.so

echo "Copying libmagparse.so to $GEMSTONE/lib/"
chmod +w $GEMSTONE/lib
rm -f $GEMSTONE/lib/libmagparse-3.1.0.2.1-64.so
cp libmagparse.so $GEMSTONE/lib/libmagparse-3.1.0.2.1-64.so
