*maglev-ast
s_a: body
  | res |
  (res := self _basicNew) bodyNode: body .
  ^ res

