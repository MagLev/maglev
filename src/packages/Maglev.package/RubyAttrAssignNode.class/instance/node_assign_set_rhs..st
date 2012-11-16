*maglev-ast
node_assign_set_rhs: rhs
  | args |
  (args := argsNode) ifNil:[ 
    argsNode :=  RubyRpCallArgs _new: rhs .
  ] ifNotNil:[
    args _append: rhs .
  ].
  ^ self

