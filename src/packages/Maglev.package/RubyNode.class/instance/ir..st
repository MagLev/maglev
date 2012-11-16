as yet unclassified
ir: aNode
  "install source position info into the receiver."
    | pos  |
	(pos := position) _isSmallInteger ifTrue:[ aNode sourceOffset: pos  "byte offset from RubyParser" ]
	              ifFalse:[  pos ifNotNil:[  pos storePositionInNode: aNode  "line number from MRI server"]].
	^ aNode

