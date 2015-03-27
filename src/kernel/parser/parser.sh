#!/bin/bash

# ANY error should stop the script and exit non-zero
# This is why we need to switch to makefiles
set -e
# turn on for verbose logging
# set -xv

# OSX HACK: hardcoded for homebrew gcc49 package
if [ -z "$CC" ]; then CC=/usr/local/bin/g++-4.9; fi
MAGLEV_HOME=$(cd $(dirname $0)/../../.. ; pwd)
GEMSTONE=$MAGLEV_HOME/gemstone
# HACK
GSVERSION=3.1.0.2.1-64
# HACK
EXT=dylib

cd $(dirname $0)

./yacc.sh

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

LDFLAGS="-Wl,-flat_namespace -Wl,-undefined -Wl,warning"
# TODO?: -Wl,-exported_symbols_list -Wl,magparse.exp

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

rm -f libmagparse.${EXT}

echo "Linking libmagparse.so"
$CC \
  -m64 -lpthread -ldl -lc -lm  -o libmagparse.${EXT} \
  $LDFLAGS \
  -Wl,-dylib \
  -L$GEMSTONE/lib \
  rubyast.o rubygrammar.o

chmod 555 libmagparse.${EXT}

echo "Copying libmagparse.${EXT} to $GEMSTONE/lib/"
chmod +w $GEMSTONE/lib
rm -f $GEMSTONE/lib/libmagparse-$GSVERSION.${EXT}
cp libmagparse.${EXT} $GEMSTONE/lib/libmagparse-3.1.0.2.1-64.${EXT}
