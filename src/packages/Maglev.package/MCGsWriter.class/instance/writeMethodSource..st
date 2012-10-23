as yet unclassified
writeMethodSource: definition
	| file |
	file := (self class: definition className).
	definition source linesDo: [:line | file nextPutAll: line; cr].
	file nextPutAll: '%'; cr; cr.

