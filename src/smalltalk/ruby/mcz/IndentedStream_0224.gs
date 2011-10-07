
set class IndentedStream
category: 'as yet unclassified'
method:
cr
	(self contents isEmpty or: [self contents last = Character cr or: [self contents last = Character tab]]) ifFalse:
		[super cr.
		self tab: self level]

%


set class IndentedStream
category: 'as yet unclassified'
method:
indent
	level _ self level + 1

%


set class IndentedStream
category: 'as yet unclassified'
method:
indent: aBlock
	self indent.
	self cr.
	aBlock value.
	self outdent

%


set class IndentedStream
category: 'as yet unclassified'
method:
indentAndEnd: aNode
	self indent: [self printNode: aNode].
	self cr; nextPutAll: 'end'

%


set class IndentedStream
category: 'as yet unclassified'
method:
level
	^ level ifNil: [0]

%


set class IndentedStream
category: 'as yet unclassified'
method:
outdent
	level _ self level - 1

%


set class IndentedStream
category: 'as yet unclassified'
method:
parenthesize: aNode
	self nextPut: $(; printNode: aNode; nextPut: $)

%


set class IndentedStream
category: 'as yet unclassified'
method:
printNode: aNode
	aNode ifNotNil: [aNode printSourceOn: self]

%

