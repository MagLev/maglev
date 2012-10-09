*maglev-ast
s_a: val b: srcOfs
  "used for RubyBreakNode, RubyNextNode"
  | res |
  (res := self _basicNew) valueNode: val ; position: srcOfs .
  ^ res

