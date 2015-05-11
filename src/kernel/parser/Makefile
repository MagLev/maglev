# A hopefully complete makefile for building the parser

OS          := $(shell uname -s)
MAGLEV_HOME := $(shell cd ../../.. ; pwd)
GEMSTONE    := $(MAGLEV_HOME)/gemstone
GSVERSION   := 3.1.0.2.1-64
CCWARN      := -Wchar-subscripts -Wcomment -Werror -Wmissing-braces -Wmultichar -Wno-aggregate-return -Wno-unused-function -Wparentheses -Wreturn-type -Wshadow -Wsign-compare -Wsign-promo -Wswitch -Wsystem-headers -Wtrigraphs -Wtrigraphs -Wuninitialized -Wunused-label -Wunused-value -Wunused-variable -Wwrite-strings $(OSCCWARN)
CCDEF       := -DFLG_FAST=1 -DNOT_JAVA_VM -D_GNU_SOURCE -D_REENTRANT
CCINC       := -I$(GEMSTONE)/include -I. $(OSCCINC)
CFLAGS      := $(CCDEF) $(CCINC) $(CCWARN) -O3 -fPIC -fcheck-new -fmessage-length=0 -fno-exceptions -fno-strict-aliasing -g -m64 -pipe -pthread -x c++
LDFLAGS     := -shared -lc -ldl -lm -lpthread -m64 $(OSLDFLAGS)

ifeq "$(OS)" "Linux"
  CC        := /usr/bin/g++
  EXT       := so
  OSLDFLAGS := -Wl,--version-script=magparse.exp -Wl,--warn-unresolved-symbols -Wl,-Bdynamic,-hlibmagparse.$(EXT) -lcrypt -lrt
  OSCCWARN  := -Wformat
  OSCCINC   := 
else ifeq "$(OS)" "Darwin"
  CC=/usr/local/bin/g++-4.9
  EXT       := dylib
  OSLDFLAGS := -Wl,-flat_namespace -Wl,-undefined -Wl,warning -Wl,-exported_symbols_list -Wl,magparse.osx.exp
  OSCCWARN  := 
  OSCCINC   := -I/usr/local/Cellar/icu4c/54.1/include
else
  $(error UNKNOWN OS: $(OS))
endif

.PHONY : help
help:
	@echo Main build targets:
	@echo
	@echo "$(MAKE) all     -- build and install"
	@echo "$(MAKE) build   -- compile and link the parser library"
	@echo "$(MAKE) install -- copy the parser library into maglev"
	@echo "$(MAKE) clean   -- delete all build products"
	@echo "$(MAKE) debug   -- show all variable values"
	@echo "$(MAKE) help    -- show this message"

.PHONY : all
all: build install

.PHONY : build
build: libmagparse.$(EXT)

.PHONY : debug
debug:
	@echo "OS          = $(OS)"
	@echo "MAGLEV_HOME = $(MAGLEV_HOME)"
	@echo "GEMSTONE    = $(GEMSTONE)"
	@echo "GSVERSION   = $(GSVERSION)"
	@echo "CCWARN      = $(CCWARN)"
	@echo "CCDEF       = $(CCDEF)"
	@echo "CCINC       = $(CCINC)"
	@echo "CFLAGS      = $(CFLAGS)"
	@echo "LDFLAGS     = $(LDFLAGS)"
	@echo "CC          = $(CC)"
	@echo "EXT         = $(EXT)"
	@echo "OSLDFLAGS   = $(OSLDFLAGS)"
	@echo "OSCCWARN    = $(OSCCWARN)"
	@echo "OSCCINC     = $(OSCCINC)"

byacc/yacc:
	cd byacc && ./configure && $(MAKE)

grammar.y: byacc/yacc

rubygrammar.c rubygrammar.h: grammar.y
	./byacc/yacc -d -g -o rubygrammar.c -P -t -v grammar.y
	if [ -d compare ]; then (cd compare && rake && cp diff*.diff ..); fi

# rubygrammar.o and rubyast.o use implicit rules

libmagparse.$(EXT): rubygrammar.o rubyast.o
	$(CC) $(LDFLAGS) rubyast.o rubygrammar.o -o libmagparse.$(EXT)

$(GEMSTONE)/lib/libmagparse-$(GSVERSION).$(EXT): libmagparse.$(EXT)
	chmod 755 $(GEMSTONE)/lib/libmagparse-$(GSVERSION).$(EXT)
	cp libmagparse.$(EXT) $(GEMSTONE)/lib/libmagparse-$(GSVERSION).$(EXT)
	chmod 555 $(GEMSTONE)/lib/libmagparse-$(GSVERSION).$(EXT)

.PHONY : install
install: $(GEMSTONE)/lib/libmagparse-$(GSVERSION).$(EXT)

.PHONY : clean
clean:
	-rm -f libmagparse.$(EXT) rubygrammar.o rubygrammar.c rubygrammar.h rubygrammar.dot rubygrammar.output rubyast.o *~
