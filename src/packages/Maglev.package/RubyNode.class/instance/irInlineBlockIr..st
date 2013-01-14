as yet unclassified
irInlineBlockIr: anIrNode 
    | block |
	block := GsComBlockNode new.
	self nextLexLevelInline:
		[:level |
		 block lexLevel: level;
		   appendStatement: anIrNode .
		].
    ^  block

