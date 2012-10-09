*maglev-ast
new_call_1: recv sel: selTok arg: argone
  "argone should never be a RubyBlockPassNode"
  | cArgs result |
  argone ifNil:[ 
    self signalError: 'internal_error new_call_1 unexpected nil arg'.
  ].
  (cArgs := RubyRpCallArgs _basicNew ) list: { argone } .
  result := ( recv class == RubySelfNode ifTrue:[ RubyFCallNode] ifFalse:[ RubyCallNode])
                 s_a: recv b: selTok symval c: cArgs  .
  result position: selTok src_offset .
  ^ result

