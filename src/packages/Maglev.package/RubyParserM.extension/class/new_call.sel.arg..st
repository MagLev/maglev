*maglev-ast
new_call: recv sel: selTok arg: args 
  | arg_cls cArgs result |
  args ifNil:[  " zero args"
    ^ self new_vcall: recv sel: selTok
  ].
  arg_cls := args class .
  arg_cls == RubyRpCallArgs ifTrue:[
    args size == 0 ifTrue:[ 
      ^ self new_vcall: recv sel: selTok
    ].
    cArgs := args
  ] ifFalse:[
    arg_cls == RubyBlockPassNode ifTrue:[
      result := (recv class ==  RubySelfNode ifTrue:[ RubyFCallNode] ifFalse:[ RubyCallNode])
                    s_a: recv b: selTok symval c: nil .
      result iterNode: args ; position: selTok src_offset .
      ^ result  
    ] ifFalse:[
      cArgs := RubyRpCallArgs _new:  args .
  ]].
  result := ( recv class == RubySelfNode ifTrue:[ RubyFCallNode] ifFalse:[ RubyCallNode])
                 s_a: recv b: selTok symval c: cArgs  .
  result position: selTok src_offset .
  ^ result

