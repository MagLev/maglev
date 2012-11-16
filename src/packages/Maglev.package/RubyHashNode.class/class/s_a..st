*maglev-ast
s_a: list
  | res aryNodeCls lst_cls |
  res := self _basicNew .
  lst_cls := list class .
  lst_cls == (aryNodeCls := RubyArrayNode) ifTrue:[
    res listNode: list
  ] ifFalse:[
     lst_cls == RubyRpCallArgs ifTrue:[ | ary |
       (ary := aryNodeCls _basicNew) list: list list .
       res listNode: ary 
     ] ifFalse:[
       RubyParserM signalError: 'RubyHashNode.s bad arg'  .     
       ^ nil
     ]
  ].
  ^ res

