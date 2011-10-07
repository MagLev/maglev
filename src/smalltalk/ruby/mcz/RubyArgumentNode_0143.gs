
set class RubyArgumentNode
category: 'accessing'
method:
definedQkind
  ^  #assignment

%


set class RubyArgumentNode
category: 'accessing'
method:
identifier

	 ^ identifier

%


set class RubyArgumentNode
category: 'accessing'
method:
identifier: aSymbol 
	identifier := aSymbol

%


set class RubyArgumentNode
category: 'converting'
method:
irNode
	^ self ir: (identifier  irNode)

%


set class RubyArgumentNode
category: 'parsetree'
method:
isSameAs: other
	^ self identifier = other identifier

%


set class RubyArgumentNode
category: 'converting'
method:
name
	^ identifier

%


set class RubyArgumentNode
category: 'printing'
method:
printSourceOn: aStream
	aStream nextPutAll: identifier

%


set class RubyArgumentNode
category: 'as yet unclassified'
method:
walkWithScope: aScope
	aScope locationForName: identifier

%


set class RubyArgumentNode
category: '*maglev-runtime'
method:
_inspect
  ^ identifier _inspect 

%

