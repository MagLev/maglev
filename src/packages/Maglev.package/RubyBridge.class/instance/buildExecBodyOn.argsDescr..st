*maglev-runtime
buildExecBodyOn: irNode argsDescr: optArgsDescr 
  | send err tooMany tooFew isErr |
  (send := GsComSendNode new )
     rcvr: (GsComVariableNode new leaf: (self argLeaf: 2 "dest method")) ;
     stSelector: #_executeInContext:args: ;
     appendArgument: (GsComVariableNode new leaf: (self argLeaf: 1 "contextObj"));
     appendArgument:(   self buildExecRest: optArgsDescr ).
  irNode appendStatement: send returnNode .

