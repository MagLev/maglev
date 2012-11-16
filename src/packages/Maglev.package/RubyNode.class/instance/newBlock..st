as yet unclassified
newBlock: aBlock
	|block|
	block := GsComBlockNode new.
	self ir: block . "to help computeLastLineNumber, Trac 708"
	^ self nextLexLevel:
		[:level |
		block lexLevel: level.
		aBlock value: block]

