*maglev-ast
s_a: srcOfs
  "used for RubyRedoNode, RubyRetryNode"
  | res |
  (res := self _basicNew) position: srcOfs .
  ^ res

