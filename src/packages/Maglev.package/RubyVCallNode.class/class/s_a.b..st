*maglev-ast
s_a: rcvr b: callSym
  | node  nodeCls  |
  nodeCls := SpecialRubySelectors at: callSym otherwise: self .
  (node := nodeCls _basicNew )
       methodName: callSym ;
       receiverNode: rcvr  .
  "caller responsible for node position: "
   ^ node

