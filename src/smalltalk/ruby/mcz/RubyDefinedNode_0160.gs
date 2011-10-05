
set class RubyDefinedNode class
category: '*maglev-ast'
method:
s_a: expr
  | res |
  (res := self _basicNew ) expressionNode: expr .
   ^ res

%


set class RubyDefinedNode
category: 'as yet unclassified'
method:
childrenForMatch
	^ {expressionNode}

%


set class RubyDefinedNode
category: '(as yet unclassified)'
method:
definedQkind
  self error:'DefinedNode should not be asking defined?' .
  ^ super definedQkind

%


set class RubyDefinedNode
category: 'accessing'
method:
expressionNode

	 ^ expressionNode

%


set class RubyDefinedNode
category: 'accessing'
method:
expressionNode: aNode
	expressionNode := aNode

%


set class RubyDefinedNode
category: 'converting'
method:
irNode
   ^  expressionNode irDefinedQNode

%


set class RubyDefinedNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope

   expressionNode walkForDefinedQ: aScope

%


set class RubyDefinedNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:defined, ', expressionNode _inspect  , $]

%

