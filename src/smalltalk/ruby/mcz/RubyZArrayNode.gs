
doit
RubyNode subclass: 'RubyZArrayNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyZArrayNode
removeallmethods
removeallclassmethods

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

