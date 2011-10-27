
set class RubyAbstractGotoNode
category: '*maglev-ast'
classmethod:
s_a: srcOfs
  "used for RubyRedoNode, RubyRetryNode"
  | res |
  (res := self _basicNew) position: srcOfs .
  ^ res

%

