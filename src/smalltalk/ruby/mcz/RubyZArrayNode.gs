
set class RubyZArrayNode
category: 'as yet unclassified'
method:
irNode
	^ self ir: GsComArrayBuilderNode new

%


set class RubyZArrayNode
category: 'as yet unclassified'
method:
printSourceOn: aStream
	aStream nextPutAll: '[]'

%

