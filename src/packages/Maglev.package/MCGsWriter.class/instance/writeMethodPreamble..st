as yet unclassified
writeMethodPreamble: definition
	| isMeta |
	isMeta := definition fullClassName ~= definition className.
	(self class: definition className)
		cr;
		nextPutAll: 'set class ', definition className; cr;
		nextPutAll: 'category: '; nextPutAll: definition category asString printString; cr;
		nextPutAll: (isMeta ifFalse: ['method:'] ifTrue: ['classmethod:']); cr.

