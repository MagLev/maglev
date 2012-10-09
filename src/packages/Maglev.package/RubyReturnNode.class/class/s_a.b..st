*maglev-ast
s_a: val b: srcOfs
  | res |
  (res := self _basicNew)
     valueNode: val ; position: srcOfs  .
  ^ res

