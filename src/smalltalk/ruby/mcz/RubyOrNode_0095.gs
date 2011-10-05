
set class RubyOrNode class
category: '*maglev-ast'
method:
s_a: left b: right
  | res |
  (res := self _basicNew) 
    firstNode: left secondNode: right.
  ^ res

%


set class RubyOrNode
category: '(as yet unclassified)'
method:
argNodes
  | s |
  ^ (s := secondNode ) ifNil:[ #() ] ifNotNil:[ { s } ]

%


set class RubyOrNode
category: '*maglev-ast'
method:
as_cond
  | f s |
  ^ RubyOrNode s_a: ((f := firstNode) ifNotNil:[ f kbegin_value as_cond ])
                 b: ((s := secondNode) ifNotNil:[ s kbegin_value as_cond ])

%


set class RubyOrNode
category: 'accessing'
method:
firstNode

	 ^ firstNode

%


set class RubyOrNode
category: 'accessing'
method:
firstNode: aNode
	firstNode := aNode

%


set class RubyOrNode
category: '*maglev-ast'
method:
firstNode: left  secondNode: right
  firstNode := left .
  secondNode := right

%


set class RubyOrNode
category: 'converting'
method:
irArgNodes
	 ^ { secondNode irBlockNodeInline: self  }

%


set class RubyOrNode
category: 'converting'
method:
isSmalltalkSend
	^ true

%


set class RubyOrNode
category: '*maglev-ast'
method:
paren
  ^ leftParen "result is nil or true"

%


set class RubyOrNode
category: '*maglev-runtime'
method:
postWalkForYield
  | s |
  firstNode postWalkForYield .
  (s := secondNode) ifNotNil:[ s postWalkForYield ].

%


set class RubyOrNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		printNode: firstNode;
		nextPutAll: ' || ';
		printNode: secondNode

%


set class RubyOrNode
category: 'converting'
method:
receiverNode
	^ firstNode

%


set class RubyOrNode
category: 'accessing'
method:
secondNode

	 ^ secondNode

%


set class RubyOrNode
category: 'accessing'
method:
secondNode: aNode
	secondNode := aNode

%


set class RubyOrNode
category: 'converting'
method:
selector
	^ #or:

%


set class RubyOrNode
category: '*maglev-ast'
method:
setParen
  "called from grammar.y via ast.h"
  leftParen := true .
  ^ self

%


set class RubyOrNode
category: 'converting'
method:
shouldOptimize
	^ true

%


set class RubyOrNode
category: '*maglev-runtime'
method:
_inspect
 ^ '[:or, ', firstNode _inspect, ', ', secondNode _inspect , $]

%

