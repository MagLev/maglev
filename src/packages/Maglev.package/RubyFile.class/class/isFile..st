as yet unclassified
isFile: aString
	"Return true iff aString is a path that maps to a file (not a directory) on the server."
	|r r1|
	r := (GsFile existsOnServer: aString) .
	r == nil ifTrue: [^ false ] .  "there was an error in existsOnServer:, i.e., the path was bogus, so not a file"
	r ifTrue:[
		r1 := GsFile isServerDirectory: aString .
		r1 == nil ifTrue: [ ^ false ] ifFalse: [ ^ r1 not ]] .
	^ false .

