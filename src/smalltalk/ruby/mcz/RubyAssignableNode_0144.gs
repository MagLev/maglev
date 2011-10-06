
set class RubyAssignableNode
category: '*maglev-ast'
classmethod:
s_a: sym b: srcOfs c: val
 "used for RubyLocalAsgnNode, RubyInstAsgnNode, RubyClassVarDeclNode"
 | res |
 (res := self _basicNew)
    name: sym ; valueNode: val ; position: srcOfs .
  ^ res

%


set class RubyAssignableNode
category: 'as yet unclassified'
method:
definedQkind
  ^  #'assignment'

%


set class RubyAssignableNode
category: 'as yet unclassified'
method:
irAssignmentNode: srcVarNode 
  | node |
  (node := GsComAssignmentNode _basicNew)
       dest: self irLeaf source:  srcVarNode .
  ^ self ir: node 

%


set class RubyAssignableNode
category: 'converting'
method:
irNode  
	^ self ir:
		(GsComAssignmentNode _basicNew
			dest: self irLeaf
			source:  valueNode irEvaluatedBlockNode )

%


set class RubyAssignableNode
category: '*maglev-ast'
method:
node_assign_set_rhs: rhs
  valueNode ifNil:[
    rhs is_void_result ifTrue:[
       RubyParserM signalError: 'void value expression'
    ].
    valueNode := rhs .
  ] ifNotNil:[
    RubyParserM signalError: ' value already assigned'
  ].
  ^ self

%


set class RubyAssignableNode
category: 'accessing'
method:
valueNode

	 ^ valueNode

%


set class RubyAssignableNode
category: 'accessing'
method:
valueNode: aNode
	valueNode := aNode

%


set class RubyAssignableNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  valueNode walkWithScope: aScope .

%

