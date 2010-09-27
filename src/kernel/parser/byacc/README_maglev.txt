Maglev modified byacc

To build the yacc executable in a fresh checkout
   ./configure.sh
   gmake

Maglev modifications include

  Deleted struct YYSTACKDATA, 
  and changed the type of yystack cells , to allow integration with
  garbage collector and so that state machine only has to maintain
  one stack pointer.  See YyStackElement and YyStackData in om.hf .
  
  A unified state table instead of multiple tables for better
  C code generation in the state machine.  C compiler should be able
  to cache one pointer to start of the unified table.

  moved a bunch of definitions to rubyparser.h . 

  moved yygrowstack implementation to the .y file .

