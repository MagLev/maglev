*maglev-ast
s_a: sym
  "used for RubyInstVarNode, RubyLocalVarNode, RubyClassVarNode"
  | res |
  (res := self _basicNew ) name: sym .
  ^ res

