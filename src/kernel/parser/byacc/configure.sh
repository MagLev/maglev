#! /bin/bash

OSTYPE = /usr/bin/bash -norc -noprofile -c 'echo $OSTYPE'

if [ $OSTYPE = "solaris" ]; then
  export CXX="/opt/sunstudio12.1/bin/CC -features=no%except"
  export CC="/opt/sunstudio12.1/bin/cc "
  export CFLAGS="-g "
  ./configure
  exit 0
fi
if  [ $OSTYPE = "linux" ]; then
  # linux default to gcc
  export CFLAGS="-O2 -Wall -Werror"
  ./configure
  exit 0
fi
echo "unsupported OSTYPE $OSTYPE"
exit 1
