
doit
RubyNode subclass: 'RubyToAryNode'
	instVarNames: #( node)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyToAryNode
removeallmethods
removeallclassmethods

set class RubyToAryNode
category: 'accessing'
method:
irNode
    "this path not used by ParAsgn and maybe not used at all" 
	^ node irNode

%


set class RubyToAryNode
category: 'as yet unclassified'
method:
isToAryNode
  ^  true

%


set class RubyToAryNode
category: 'as yet unclassified'
method:
list
  ^ { node }

%


set class RubyToAryNode
category: 'accessing'
method:
node

	 ^ node

%


set class RubyToAryNode
category: 'accessing'
method:
node: aNode
	node := aNode

%


set class RubyToAryNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
    node walkWithScope: aScope

%


set class RubyToAryNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:to_ary, ', node _inspect , $]

%

