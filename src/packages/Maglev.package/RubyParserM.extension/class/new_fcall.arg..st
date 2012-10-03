*maglev-ast
new_fcall: selTok arg: arg
  "receiver is implicit self"
  | result arg_cls sym |
  arg ifNil:[
    ^ self new_vcall: RubySelfNode _basicNew sel: selTok
  ].
  arg_cls := arg class .
  arg_cls == RubyRpCallArgs ifTrue:[
    arg size == 0 ifTrue:[
      ^ self new_vcall: RubySelfNode _basicNew sel: selTok
    ].
    sym := self check_selector: selTok .
    result := RubyFCallNode s_a: RubySelfNode _basicNew b: sym c: arg .
  ] ifFalse:[
    arg_cls == RubyBlockPassNode  ifTrue:[
      sym := self check_selector: selTok  .
      result := RubyFCallNode s_a: RubySelfNode _basicNew b: sym c: nil .
      result iterNode: arg 
    ] ifFalse:[ | cArgs |
      sym := self check_selector: selTok  .
      (cArgs := RubyRpCallArgs _basicNew) list: { arg } .
      result := RubyFCallNode s_a: RubySelfNode _basicNew b: sym c: cArgs 
  ]].
  result position: selTok src_offset .
  ^ result

