
set class RubyDVarNode
category: '*maglev-runtime'
method:
determineDynamic
  ^ 2

%


set class RubyDVarNode
category: '*maglev-runtime'
method:
irLeaf
    ^ location  leaf

%


set class RubyDVarNode
category: 'as yet unclassified'
method:
isSameAs: other
	^ other name = self name
		and: [other location = self location]

%


set class RubyDVarNode
category: 'accessing'
method:
location

	 ^ location

%


set class RubyDVarNode
category: 'accessing'
method:
location: aNumber
	location := aNumber

%


set class RubyDVarNode
category: 'accessing'
method:
name

	 ^ name

%


set class RubyDVarNode
category: 'accessing'
method:
name: aSymbol 
	name := aSymbol .

%


set class RubyDVarNode
category: 'printing'
method:
printSourceOn: aStream
	aStream nextPutAll: name

%


set class RubyDVarNode
category: 'as yet unclassified'
method:
walkWithScope: aScope
	location := aScope locationForName: name

%

