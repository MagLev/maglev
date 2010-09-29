#! /bin/bash
#  MTYPE should normally come from environment set by build.pl

export MTYPE=$machine

if [ $MTYPE = "13" ]; then
  export CXX="/opt/sunstudio12.1/bin/CC -features=no%except"
  export CC="/opt/sunstudio12.1/bin/cc "
  export CFLAGS="-g "
  ./configure
  exit 0
fi
if  [ $MTYPE = "50" ]; then
  # linux default to gcc
  export CFLAGS="-O2 -Wall -Werror"
  ./configure
  exit 0
fi
echo "unsupported MTYPE $MTYPE"
exit 1
