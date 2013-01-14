as yet unclassified
sourceString
	| stream |
	stream := IndentedStream on: String new.
	self printSourceOn: stream.
	^ stream contents

