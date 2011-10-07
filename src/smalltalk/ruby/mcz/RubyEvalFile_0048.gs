
set class RubyEvalFile
category: 'as yet unclassified'
method:
fileName: aFileName
	"Ruby eval sometimes passes in a file name to use"
	fileName := aFileName .

%


set class RubyEvalFile
category: 'as yet unclassified'
method:
fullPath
	^ (fileName == nil) ifTrue: [ 'eval' ] ifFalse: [ fileName ]

%


set class RubyEvalFile
category: 'as yet unclassified'
method:
loadName
	^ (fileName == nil) ifTrue: [ 'eval' ] ifFalse: [ fileName ]

%


set class RubyEvalFile
category: 'as yet unclassified'
method:
path
	^ (fileName == nil) ifTrue: [ 'eval' ] ifFalse: [ fileName ]

%


set class RubyEvalFile
category: 'as yet unclassified'
method:
source
	^ source

%


set class RubyEvalFile
category: 'as yet unclassified'
method:
source: aString
	source := aString

%

