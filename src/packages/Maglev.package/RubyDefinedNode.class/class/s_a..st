*maglev-ast
s_a: expr
  | res |
  (res := self _basicNew ) expressionNode: expr .
   ^ res

