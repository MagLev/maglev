Maglev modified version of the 'melbourne' Ruby parser .

grammar.y copied from Rubinius source code on August 6 , 2010 
and then edited.

==================

To create the generated parser files
  
  build the executable  byacc/yacc per byacc/README_maglev.txt

  ./yacc.sh   # runs byacc/yacc and copies generated files
              #  to the svn/src directory 

Then to get changes to show up in your Smalltalk VM,
rerun the  slow or fast compile step of the Smalltalk product.

==========================
algorithmic changes to the Rubinius grammar
  
Cells in the parser state machine's stack are per YyStackElement in om.hf 
Actions within the grammar create objects structurally with om:: calls
or call Smalltalk methods via IntRecurFromPrim  to execute smalltalk
code to construct AST nodes.

Added ary_ref to the grammar,
   ary_ref         : '[' aref_args ']'
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
