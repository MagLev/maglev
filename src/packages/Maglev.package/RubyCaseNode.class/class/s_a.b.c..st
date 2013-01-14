*maglev-ast
s_a: exprArg b: body  c: ofs
  "body is a WhenNode  "
  | node  |
  (node := self _basicNew )
     caseNode: exprArg caseBody: body  ;
     position: ofs .
  ^ node

