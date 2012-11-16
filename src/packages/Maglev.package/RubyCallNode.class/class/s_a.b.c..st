*maglev-ast
s_a: rcvr b: callSym c: args
  | node nodeCls | 
  nodeCls :=  SpecialRubySelectors at: callSym otherwise: self .
  (node := nodeCls _basicNew)
     methodName: callSym ;
     argsNodeRp: args ;
     receiverNode: rcvr .
  "caller responsible for node position: "
  ^ node

