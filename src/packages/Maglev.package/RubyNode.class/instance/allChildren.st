as yet unclassified
allChildren
	^ self childrenForMatch select: [:ea | ea isKindOf: RubyNode]

