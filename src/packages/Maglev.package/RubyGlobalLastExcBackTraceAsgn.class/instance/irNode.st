*maglev-runtime
irNode
  | leaf node  |
  (leaf := self irLeaf) 
    ifNotNil: [ | varNode |
      varNode := GsComVariableNode new.
      varNode leaf: self irLeaf.
      node := GsComSendNode new
        rcvr: varNode;
        rubySelector: #'set_backtrace#1__';
        appendArgument: valueNode irNode]
    ifNil: [
      "TODO: Raise Ruby ArgumentError directly"
       node := GsComSendNode new
         rcvr: (GsComVariableNode globalNamed: #ArgumentError inDict: Globals);
         rubySelector: #'_set_backtrace_failed#0__'].
   self ir: node .  
   ^ node 
