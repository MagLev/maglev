*maglev-ast
new_until: block expr: exprArg ofs: srcOfs
  | expr |
  exprArg class == RubyNotNode ifTrue:[
     expr := exprArg conditionNode
  ] ifFalse:[
     expr := RubyNotNode s_a: exprArg .
  ].
  expr position: srcOfs .
  ^ self new_while: block expr: expr ofs: srcOfs

