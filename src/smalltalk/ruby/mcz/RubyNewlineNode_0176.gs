
set class RubyNewlineNode class
category: 'parsetree'
method:
nodeOnTokenStream: aStream
              "8 Aug 2008   no senders found"
	aStream next; next.
	^ self new

%


set class RubyNewlineNode
category: 'as yet unclassified'
method:
asMethodDefNode
  ^ nextNode asMethodDefNode

%


set class RubyNewlineNode
category: 'printing'
method:
asString
  ^ super asString , ' line ' , position line asString

%


set class RubyNewlineNode
category: 'converting'
method:
irForNode
	^ nextNode irForNode

%


set class RubyNewlineNode
category: 'as yet unclassified'
method:
irForNodeWillBeInline
  ^ nextNode irForNodeWillBeInline

%


set class RubyNewlineNode
category: 'converting'
method:
irIterResult
	^ nextNode irIterResult

%


set class RubyNewlineNode
category: 'converting'
method:
irNode
	^ nextNode irNode

%


set class RubyNewlineNode
category: '*maglev-runtime'
method:
irNodeListInto: blockIr
    nextNode irNodeListInto: blockIr

%


set class RubyNewlineNode
category: 'parsetree-test'
method:
isSameAs: other
	^ self position isNil or: [other position isNil] or: [self position startLine = other position startLine]

%


set class RubyNewlineNode
category: 'accessing'
method:
nextNode

	 ^ nextNode

%


set class RubyNewlineNode
category: 'accessing'
method:
nextNode: aNode
	nextNode := aNode

%


set class RubyNewlineNode
category: 'printing'
method:
printSourceOn: aStream
	aStream cr; printNode: nextNode

%


set class RubyNewlineNode
category: 'as yet unclassified'
method:
setForLoopNotInline
  nextNode setForLoopNotInline

%


set class RubyNewlineNode
category: 'as yet unclassified'
method:
walkIterWithScope: aScope
  ^ nextNode walkIterWithScope: aScope

%


set class RubyNewlineNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
    nextNode walkWithScope: aScope

%

