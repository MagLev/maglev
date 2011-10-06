
set class RubyArgsPushNode
category: '(as yet unclassified)'
method:
argNodes
    walked := true .
    ^ list ifNil:[ #() ]

%


set class RubyArgsPushNode
category: 'as yet unclassified'
method:
hasRestArg
  ^ false

%


set class RubyArgsPushNode
category: 'as yet unclassified'
method:
printSourceOn: aStream
	aStream nextPut: $[.
	self printArgsOn: aStream.
	aStream nextPut: $]

%


set class RubyArgsPushNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
   walked ifNil:[ 
     walked := true .
     ^ super walkWithScope: aScope
   ]

%

