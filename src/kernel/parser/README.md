# Maglev Parser

Maglev modified version of the 'melbourne' Ruby parser .

grammar.y copied from Rubinius source code on August 6 , 2010 and then edited.

### Create generated parser files

build the executable `byacc/yacc` per `byacc/README_maglev.txt`

```shell
./yacc.sh   # runs byacc/yacc to produce generated file rubygrammar.c
```

Then create a rake or make task to do the following steps

* compile rubygrammar.c
* compile rubyast.c
* link libmagparse.so
* install libmagparse.so in $GEMSTONE/lib

After all of this is worked out on Linux,
we will publish a  maglev 3.1.0.2.1 server for Mac and
additional Rake file changes will be needed for Mac .


### Linux compile commands with full C debugging

```shell
/usr/bin/g++ -fmessage-length=0 -fcheck-new \
-Wformat -Wtrigraphs -Wcomment \
-Wsystem-headers -Wtrigraphs -Wno-aggregate-return -Wswitch -Wshadow -Wunused-value \
-Wunused-variable -Wunused-label -Wno-unused-function -Wchar-subscripts -Wmissing-braces \
-Wmultichar -Wparentheses -Wsign-compare -Wsign-promo -Wwrite-strings -Wreturn-type \
  -g -DFLG_DEBUG=1 \
  -m64  -pipe -D_REENTRANT -DNOT_JAVA_VM -D_GNU_SOURCE -pthread  -fPIC \
-fno-strict-aliasing -fno-exceptions \
-I. -I$GEMSTONE/include -x c++ \
-c rubygrammar.c -o rubygrammar.o

/usr/bin/g++ -fmessage-length=0 -fcheck-new \
-Wformat -Wtrigraphs -Wcomment \
-Wsystem-headers -Wtrigraphs -Wno-aggregate-return -Wswitch -Wshadow -Wunused-value \
-Wunused-variable -Wunused-label -Wno-unused-function -Wchar-subscripts -Wmissing-braces \
-Wmultichar -Wparentheses -Wsign-compare -Wsign-promo -Wwrite-strings -Wreturn-type \
  -g -DFLG_DEBUG=1 \
  -m64  -pipe -D_REENTRANT -DNOT_JAVA_VM -D_GNU_SOURCE -pthread  -fPIC \
-fno-strict-aliasing -fno-exceptions \
-I. -I$GEMSTONE/include -x c++ \
-c rubyast.c -o rubyast.o
```

### Linux compile commands with C optimizer turned on

```shell
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
```

### Linux link libmagparse.so

```shell
rm -f libmagparse.so
/usr/bin/g++ -shared -Wl,-Bdynamic,-hlibmagparse.so  -Wl,--version-script=magparse.exp \
    rubyast.o rubygrammar.o  \
    -m64 -lpthread -lcrypt -ldl -lc -lm -lrt -o libmagparse.so \
    -Wl,--warn-unresolved-symbols
chmod 555 libmagparse.so
```

install the libmagparse.so in the $GEMSTONE/lib directory
As downloaded from seaside.gemtalksystems.com ,
the lib directory contains a libmagparse-3.1.0.2.1-64.so
which is a copy of libmagparsedummy.so from the server build.
libmagparsedummy.so is sufficient to run pure smalltalk code.
This  cp  will install the actual libmagparse.so .

```shell
chmod +w $GEMSTONE/lib
rm -f $GEMSTONE/lib/libmagparse-3.1.0.2.1-64.so
cp libmagparse.so $GEMSTONE/lib/libmagparse-3.1.0.2.1-64.so
```

==========================

### Algorithmic changes to the Rubinius grammar

Cells in the parser state machine's stack are per YyStackElement in om.hf
Actions within the grammar create objects structurally with om:: calls
or call Smalltalk methods via IntRecurFromPrim  to execute smalltalk
code to construct AST nodes.

Added ary_ref to the grammar,
`ary_ref         : '[' aref_args ']'`
so that the grammar action can do the rParenLexPop() for the ']'

Moved the COND_LEXPOP and CMDARG_LEXPOP for ']' '}' ')' out of the
lexer and into the grammar actions (see calls to rParenLexPop() )
to fix  CMDARG stack overflow seen in running various ruby spec files.
Doing the pop in the lexer results in pops which are out of order
with the grammar actions. Specifically the cmdarg_stack.restoreFromSi
in the 'command_args:' production can overwrite the result
of a pop within the lexer.

Added ps->charTypes  character table to optimize character type
tests within the lexer.  See initCharTypes() in grammar.y .
