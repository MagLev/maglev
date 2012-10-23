as yet unclassified
newBlock: aBlock isInline: inlineBool
^  inlineBool ifTrue:[ self newInlineBlock: aBlock ]
	        ifFalse:[ self newBlock: aBlock ]

