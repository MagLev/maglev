
set class RubyOpAsgnAndNode
category: '(as yet unclassified)'
method:
argNodes
  | s |
  ^ (s := secondNode ) ifNil:[ #() ] ifNotNil:[ { s } ]

%


set class RubyOpAsgnAndNode
category: 'as yet unclassified'
method:
definedQkind
  ^ 'assignment'

%


set class RubyOpAsgnAndNode
category: 'accessing'
method:
firstNode

	 ^ firstNode

%


set class RubyOpAsgnAndNode
category: '*maglev-runtime'
method:
firstNode: nod
    firstNode := nod

%


set class RubyOpAsgnAndNode
category: 'accessing'
method:
irArgNodes
	 ^ { self newBlock:
		 [:block |
		 block appendStatement: secondNode irArgNode.
		 block] }

%


set class RubyOpAsgnAndNode
category: 'accessing'
method:
isSmalltalkSend
	^ true

%


set class RubyOpAsgnAndNode
category: 'accessing'
method:
receiverNode
	^ firstNode

%


set class RubyOpAsgnAndNode
category: 'accessing'
method:
secondNode

	 ^ secondNode

%


set class RubyOpAsgnAndNode
category: '*maglev-runtime'
method:
secondNode: nod
    secondNode := nod

%


set class RubyOpAsgnAndNode
category: 'accessing'
method:
selector
	^ #and:

%


set class RubyOpAsgnAndNode
category: 'accessing'
method:
shouldOptimize
	^ true

%


set class RubyOpAsgnAndNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  super walkWithScope: aScope .
  firstNode postWalkForYield .

%


set class RubyOpAsgnAndNode
category: '*maglev-runtime'
method:
_inspect
 ^ '[:op_asgn_and, ', firstNode _inspect, ', ', secondNode _inspect , $]

%

