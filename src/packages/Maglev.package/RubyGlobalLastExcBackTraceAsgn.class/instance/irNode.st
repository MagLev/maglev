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
		"TODO: Raise Ruby ArgumentError"
		self error: 'ArgumentError: $! not set'].
   self ir: node .  
   ^ node 
