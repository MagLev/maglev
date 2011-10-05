
set class RubyAndNode class
category: '*maglev-ast'
method:
s_a: left b: right
  | res |
  (res := self _basicNew) 
    firstNode: left  secondNode: right.
  ^ res

%


set class RubyAndNode
category: '(as yet unclassified)'
method:
argNodes
  | s |
  ^ (s := secondNode) ifNil:[ #() ] ifNotNil:[ { s } ]

%


set class RubyAndNode
category: '*maglev-ast'
method:
as_cond
  | f s |
  ^ RubyAndNode s_a: ((f := firstNode) ifNotNil:[ f kbegin_value as_cond ])
                  b: ((s := secondNode) ifNotNil:[ s kbegin_value as_cond ])

%


set class RubyAndNode
category: 'accessing'
method:
firstNode

	 ^ firstNode

%


set class RubyAndNode
category: 'accessing'
method:
firstNode: aNode
	firstNode := aNode

%


set class RubyAndNode
category: '*maglev-ast'
method:
firstNode: left  secondNode: right
  firstNode := left .
  secondNode := right

%


set class RubyAndNode
category: 'converting'
method:
irArgNodes
	^ { secondNode irBlockNodeInline: self  }

%


set class RubyAndNode
category: 'converting'
method:
isSmalltalkSend
	^ true

%


set class RubyAndNode
category: '*maglev-ast'
method:
paren
  ^ leftParen "result is nil or true"

%


set class RubyAndNode
category: '*maglev-runtime'
method:
postWalkForYield
  | s |
  firstNode postWalkForYield .
  (s := secondNode) ifNotNil:[ s postWalkForYield ].

%


set class RubyAndNode
category: 'converting'
method:
receiverNode
	^ firstNode

%


set class RubyAndNode
category: 'accessing'
method:
secondNode

	 ^ secondNode

%


set class RubyAndNode
category: 'accessing'
method:
secondNode: aNode
	secondNode := aNode

%


set class RubyAndNode
category: 'converting'
method:
selector
	^ #and:

%


set class RubyAndNode
category: '*maglev-ast'
method:
setParen
  "called from grammar.y via ast.h"
  leftParen := true .
  ^ self

%


set class RubyAndNode
category: 'converting'
method:
shouldOptimize
	^ true

%


set class RubyAndNode
category: '*maglev-runtime'
method:
_inspect
 ^ '[:and, ' , firstNode _inspect , ', ' , secondNode _inspect, $]

%

