*maglev-ast
s_a: args b: body c: srcOfs
  | res |
  (res := self _basicNew) 
     _varNode: args ; position: srcOfs ;
     bodyNode: body .
  ^ res

