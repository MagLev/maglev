
doit
RubyAssignableNode subclass: 'RubyDAsgnNode'
	instVarNames: #( location name isBlockArg)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDAsgnNode
removeallmethods
removeallclassmethods

set class RubyDAsgnNode
category: 'converting'
method:
buildBlockArgumentsOn: irNode

  isBlockArg ifFalse:[ self error:'inconsistent isBlockArg in Dasgn'].
  irNode appendArg:  self irLeaf

%


set class RubyDAsgnNode
category: 'as yet unclassified'
method:
buildIrLeafsInto: anArray
   anArray add:  self irLeaf

%


set class RubyDAsgnNode
category: '*maglev-runtime'
method:
irLeaf
  ^  location  leaf

%


set class RubyDAsgnNode
category: 'as yet unclassified'
method:
irNode  
   isBlockArg ifTrue:[ self error:'irNode illegal for DAsgn as block arg'].
	^ self ir:
		(GsComAssignmentNode _basicNew
			dest: self irLeaf
			source:  valueNode irLocalAsgnValue  )

%


set class RubyDAsgnNode
category: 'as yet unclassified'
method:
isSameAs: other
	^ other name = self name
		and: [other location = self location]

%


set class RubyDAsgnNode
category: 'as yet unclassified'
method:
isSingleIterArg
  ^ true

%


set class RubyDAsgnNode
category: 'accessing'
method:
location

	 ^ location

%


set class RubyDAsgnNode
category: 'accessing'
method:
location: aNumber
	location := aNumber

%


set class RubyDAsgnNode
category: 'accessing'
method:
name

	 ^ name

%


set class RubyDAsgnNode
category: 'accessing'
method:
name: aSymbol  
  "returns receiver"
	name := aSymbol .
	isBlockArg := false .

%


set class RubyDAsgnNode
category: 'printing'
method:
printSourceOn: aStream
	aStream nextPutAll: name.
	valueNode ifNotNil:
		[aStream nextPutAll: ' = '; printNode: valueNode]

%


set class RubyDAsgnNode
category: 'as yet unclassified'
method:
setIsBlockArg
  isBlockArg := true

%


set class RubyDAsgnNode
category: 'as yet unclassified'
method:
walkWithScope: aScope
   isBlockArg 
      ifTrue:[ location := aScope locationForBlockArg: name ]
     ifFalse:[  location := aScope locationForName: name ].
   super walkWithScope: aScope

%


set class RubyDAsgnNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:dasgn, :', name, ', ', valueNode _inspect , $]

%

