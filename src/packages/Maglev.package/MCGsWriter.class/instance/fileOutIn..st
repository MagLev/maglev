as yet unclassified
fileOutIn: path
	| dir loadOrderFile |
	dir := (FileDirectory on: path)
			assureExistence;
			deleteLocalFiles;
			yourself.
	loadOrderFile := dir forceNewFileNamed: 'filein.gs'.

	fileStreams do: [:assoc || className count stream filename |
		"for info on :assoc, see #class:"
		className := assoc key.
		count := assoc value first = 0 ifTrue: [''] ifFalse: [assoc value first asString].
		stream := assoc value last.
		filename := className, count, '.gs'.

		loadOrderFile nextPutAll: 'input $MAGLEV_HOME/src/smalltalk/ruby/mcz/'; nextPutAll: filename; lf.
		(dir forceNewFileNamed: filename) nextPutAll: stream contents; close].
	loadOrderFile close.
