
set class RubyAbstractGotoNode class
category: '*maglev-ast'
method:
s_a: srcOfs
  "used for RubyRedoNode, RubyRetryNode"
  | res |
  (res := self _basicNew) position: srcOfs .
  ^ res

%

