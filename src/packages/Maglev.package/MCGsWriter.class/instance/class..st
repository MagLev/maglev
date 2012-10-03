as yet unclassified
class: aName
	"associates the class name with a {index . stream} pair"
	"if the requested class isn't the last one, but we already have a file stream for it,
	 that means that we are processing a definition that depends on something to be loaded
	 in between, so we create a new file for that"
	fileStreams last key = aName ifFalse: ["we haven't worked on this last"
		| count |
		count := fileStreams count: [:each | each key = aName].
		fileStreams add: aName -> {count . String new writeStream}].
	^ fileStreams last "requested class assoc" value "{count . stream}" last "stream"
