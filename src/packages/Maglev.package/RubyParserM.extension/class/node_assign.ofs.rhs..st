*maglev-ast
node_assign: lhs ofs: srcOfs rhs: rhsArg
  | rhs nLhs |
  lhs ifNil:[ ^ nil ].
  rhs := rhsArg kbegin_value .
  nLhs := lhs node_assign_set_rhs: rhs . " may raise error"
  nLhs == lhs ifFalse:[ nLhs _becomeMinimalChecks: lhs ].
  ^ lhs

