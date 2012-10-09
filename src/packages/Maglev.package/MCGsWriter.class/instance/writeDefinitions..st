as yet unclassified
writeDefinitions: aCollection
	fileStreams := OrderedCollection with: 'class_definitions' -> {0. String new writeStream}.
	(MCDependencySorter sortItems: aCollection)
		do: [:ea | ea accept: self].

