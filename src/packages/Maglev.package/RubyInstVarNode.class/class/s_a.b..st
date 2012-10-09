*maglev-ast
s_a: sym b: srcOfs

  | res |
  (res := self _basicNew ) name: sym ; position: srcOfs .
  ^ res

