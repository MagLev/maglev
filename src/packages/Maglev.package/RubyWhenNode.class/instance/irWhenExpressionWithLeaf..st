*maglev-runtime
irWhenExpressionWithLeaf: aLeaf
   "used to handle  *list  form of argument to a   when    keyword"
      "ruby_selector_suffix dependent"
   | node arrBuilder |
  arrBuilder := expressionNodes irNode .
  self ir: arrBuilder .
  (node := GsComSendNode new)  rcvr: arrBuilder.
  aLeaf ifNotNil:[
      node    rubySelector:  #'__anySatisfyCaseLeaf#1__'   ;
        appendArgument: (GsComVariableNode new leaf: aLeaf) 
  ] ifNil:[
      node rubySelector: #'__anySatisfyCaseTrue#0__' 
  ].
  self ir: node .
  ^ node 

