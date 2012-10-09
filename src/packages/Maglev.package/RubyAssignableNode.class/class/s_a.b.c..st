*maglev-ast
s_a: sym b: srcOfs c: val
 "used for RubyLocalAsgnNode, RubyInstAsgnNode, RubyClassVarDeclNode"
 | res |
 (res := self _basicNew)
    name: sym ; valueNode: val ; position: srcOfs .
  ^ res

