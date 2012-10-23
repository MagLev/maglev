as yet unclassified
newInlineBlock: aBlock
	| block |
	block := GsComBlockNode new.
	^ self nextLexLevelInline:
		[:level |
		block lexLevel: level.
		aBlock value: block]

