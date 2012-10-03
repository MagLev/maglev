*maglev-ast
get_match_node: lhs rhs: rhs ofs: srcOfs
  | res |
  lhs ifNotNil:[ | l_cls |
    l_cls := lhs class .
    l_cls == RubyDRegexpNode ifTrue:[
      (res := RubyMatch2Node s_a: lhs b: rhs )
        position: srcOfs .
      ^ res
    ].
    l_cls == RubyRegexpNode ifTrue:[
      (res := RubyMatch2Node s_a: lhs b: rhs )
        position: srcOfs .
      ^ res
    ].
  ].
  rhs ifNotNil:[ | r_cls |
    r_cls := rhs class .
    r_cls == RubyDRegexpNode ifTrue:[
      (res := RubyMatch2Node s_a: rhs b: lhs) " we use Match2Node for :match3"
         position: srcOfs .
      ^ res .
    ].
    r_cls == RubyRegexpNode ifTrue:[
      (res := RubyMatch2Node s_a: rhs  b: lhs)
               position: srcOfs .
      ^ res .
    ].
  ].
  res := RpNameToken new: #'=~' position: srcOfs .
  ^ self new_call_1: lhs  sel: res arg: rhs  .

