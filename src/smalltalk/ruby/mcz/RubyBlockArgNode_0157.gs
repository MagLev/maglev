
set class RubyBlockArgNode
category: '*maglev-ast'
classmethod:
s_a: sym
  | res |
  (res := self _basicNew) name: sym .
  ^ res

%


set class RubyBlockArgNode
category: 'parsetree'
method:
isBlockArgNode
	^ true

%


set class RubyBlockArgNode
category: 'parsetree'
method:
isSameAs: other
	self flag: #doWeNeedCount.
	^ self name = other name

%


set class RubyBlockArgNode
category: 'accessing'
method:
name

	 ^ name

%


set class RubyBlockArgNode
category: 'accessing'
method:
name: aSymbol
	name := aSymbol

%


set class RubyBlockArgNode
category: 'printing'
method:
printSourceOn: aStream
	(((aStream contents last: 2) = ', ') or: [aStream contents last = '(']) ifFalse:
		[aStream nextPutAll: ', '].
	aStream nextPutAll: '&', name asString

%


set class RubyBlockArgNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:block_arg, ', name , $]

%

