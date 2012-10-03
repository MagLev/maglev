*maglev-ast
new_while: blockArg expr: exprArg ofs: srcOfs
  | preBool expr res block |
  preBool := true . 
  (block := blockArg) class == RubyBeginNode ifTrue:[
     block := block kbegin_value .
     preBool := false
  ].
  expr := exprArg kbegin_value as_cond .
  expr class == RubyNotNode ifTrue:[
    res := RubyUntilNode s_forRp: expr conditionNode block: block bool: preBool
  ] ifFalse:[
    res := RubyWhileNode s_forRp: expr block: block bool: preBool
  ]. 
  ^ res position: srcOfs

