#!/bin/bash

if [ -z "$CC" ]; then CC=/usr/bin/g++; fi
PARSERDIR=$(cd $(dirname $0) ; pwd)
MAGLEV_HOME=$(cd $(dirname $0)/../../.. ; pwd)
GEMSTONE=$MAGLEV_HOME/gemstone
# HACK
GSVERSION=3.1.0.2.1-64

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

CCWARN="-Wchar-subscripts -Wcomment -Werror -Wformat -Wmissing-braces -Wmultichar -Wno-aggregate-return -Wno-c++11-extensions -Wno-constant-logical-operand -Wno-conversion -Wno-deprecated-declarations -Wno-format -Wno-invalid-offsetof -Wno-self-assign-field -Wno-unused-function -Wparentheses -Wreturn-type -Wshadow -Wsign-compare -Wsign-promo -Wswitch -Wsystem-headers -Wtrigraphs -Wunused-label -Wunused-value -Wunused-variable -Wwrite-strings"

# TODO: most of these look like legacy copy&paste.
CCDEF="-DFLG_DEBUG=1 -D_XOPEN_SOURCE -D_REENTRANT -DNOT_JAVA_VM -D_GNU_SOURCE -D_LARGEFILE64_SOURCE -Dlint"

$CC $CCWARN $CCDEF \
  -g -m64 -pipe -pthread -fmessage-length=0 -fPIC -fno-strict-aliasing -fno-exceptions \
  -I. -I$GEMSTONE/include -I/usr/local/Cellar/icu4c/54.1/include \
  -x c++ \
  -c rubygrammar.c -o rubygrammar.o

if test $? -ne 0; then
  echo "compiling rubygrammar.o failed"
  exit 1
fi

echo "Compiling rubyast.o"
$CC $CCWARN $CCDEF \
  -g -m64 -pipe -pthread -fmessage-length=0 -fPIC -fno-strict-aliasing -fno-exceptions \
  -I. -I$GEMSTONE/include -I/usr/local/Cellar/icu4c/54.1/include \
  -x c++ \
  -c rubyast.c -o rubyast.o

if test $? -ne 0; then
  echo "compiling rubyast.o failed"
  exit 1
fi

rm -f libmagparse.so

set -xve

echo "Linking libmagparse.so"
$CC \
  -shared \
  -m64 -lpthread -ldl -lc -lm -o libmagparse.so \
  -L$GEMSTONE/lib \
  -lgcilnk-$GSVERSION \
  rubyast.o rubygrammar.o

chmod 555 libmagparse.so

echo "Copying libmagparse.so to $GEMSTONE/lib/"
chmod +w $GEMSTONE/lib
rm -f $GEMSTONE/lib/libmagparse-$GSVERSION.so
cp libmagparse.so $GEMSTONE/lib/libmagparse-3.1.0.2.1-64.so
