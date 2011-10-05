
set class RubyAbstractVarNode class
category: '*maglev-ast'
method:
s_a: sym
  "used for RubyInstVarNode, RubyLocalVarNode, RubyClassVarNode"
  | res |
  (res := self _basicNew ) name: sym .
  ^ res

%


set class RubyAbstractVarNode
category: 'as yet unclassified'
method:
irNode
	^ self ir: (GsComVariableNode new leaf: self irLeaf)

%

