*maglev-ast
new_if: condArg t: trueBlock f: falseBlock ofs: srcOfs
  | c res |
  c := condArg as_cond .
  c class == RubyNotNode ifTrue:[ 
    res := RubyIfNode s_a: c conditionNode b: falseBlock c: trueBlock 
  ] ifFalse:[
    res := RubyIfNode s_a: c b: trueBlock c: falseBlock .
  ].
  ^ res position: srcOfs

