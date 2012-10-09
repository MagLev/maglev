*maglev-runtime
irWhenExpressionWithLeaf: aLeaf
      "ruby_selector_suffix dependent"
  | node |
  aLeaf ifNotNil:[
    (node := GsComSendNode new)
        rcvr:  self irNode ;
        rubySelector:  #'===#1__'  ;
        appendArgument: (GsComVariableNode new leaf: aLeaf) .
    self ir: node .
    ^ node 
  ] ifNil:[  "parsing   case with no target expression" 
     ^ self irNode 
  ].

