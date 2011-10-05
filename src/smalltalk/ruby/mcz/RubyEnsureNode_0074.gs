
set class RubyEnsureNode class
category: '*maglev-ast'
method:
s_a: argBlock b: srcOfs
  | res |
  (res := self _basicNew )
     ensureNode: (argBlock ifNil:[ RubyNilNode _basicNew]) ;
     position: srcOfs .
  ^ res

%


set class RubyEnsureNode
category: '(as yet unclassified)'
method:
argNodes
  | s |
  ^ (s := ensureNode ) ifNil:[ #() ] ifNotNil:[ { s } ]

%


set class RubyEnsureNode
category: 'accessing'
method:
bodyNode

	 ^ bodyNode

%


set class RubyEnsureNode
category: 'accessing'
method:
bodyNode: aNode
	bodyNode := aNode

%


set class RubyEnsureNode
category: 'accessing'
method:
ensureNode

	 ^ ensureNode

%


set class RubyEnsureNode
category: 'accessing'
method:
ensureNode: aNode
	ensureNode := aNode

%


set class RubyEnsureNode
category: 'converting'
method:
irArgNodes
	^ { ensureNode irBlockNode: self  }

%


set class RubyEnsureNode
category: 'converting'
method:
irReceiverNode
	^ bodyNode irBlockNode: self

%


set class RubyEnsureNode
category: 'converting'
method:
isSmalltalkSend
	^ true

%


set class RubyEnsureNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		printNode: bodyNode;
		outdent; cr;
		nextPutAll: 'ensure';
		indent; cr;
		printNode: ensureNode

%


set class RubyEnsureNode
category: 'converting'
method:
receiverNode
	^ bodyNode

%


set class RubyEnsureNode
category: '*maglev-runtime'
method:
selector
 ^ isStEnsure ifTrue:[ #ensure: ] ifFalse:[ #rubyEnsure: ]

%


set class RubyEnsureNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  isStEnsure := aScope inBootstrap .
  ^ super  walkWithScope: aScope .

%


set class RubyEnsureNode
category: '*maglev-runtime'
method:
_inspect
 ^ '[:ensure, ', bodyNode _inspect, ', ',  ensureNode _inspect , $]

%

