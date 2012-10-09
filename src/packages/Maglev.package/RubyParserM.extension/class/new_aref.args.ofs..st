*maglev-ast
new_aref: rcv args: refArgs ofs: srcOfs
  | args res |
  args := refArgs ifNil:[ RubyRpCallArgs _new ].
  (res := RubyCallNode s_a: rcv b: #'[]' c: args )  position: srcOfs .
  ^ res

