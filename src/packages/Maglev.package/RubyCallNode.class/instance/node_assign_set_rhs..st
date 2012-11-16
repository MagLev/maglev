*maglev-ast
node_assign_set_rhs: rhs
  rhs ifNil:[ self error:'invalid nil arg to RubyCallNode>>node_assign_set_rhs:' ].
  argsNode _append: rhs .
  ^ self

