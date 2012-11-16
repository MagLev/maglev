*maglev-runtime
comment
  ^ '
RubyVCallNode  is a call with  zero arguments  .
     
RubyFCallNode   is a call with receiver==self and at least one arg 

RubyCallNode   is a call with explicit receiver . 
 
The following eval variants exist
  RubyVCallEvalNode  (should result in ArgumentError, too few args at runtime)
  RubyFCallEvalNode
  RubyCallEvalNode
'

