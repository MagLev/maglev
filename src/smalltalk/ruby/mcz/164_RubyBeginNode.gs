
doit
RubyNode subclass: 'RubyBeginNode'
	instVarNames: #( bodyNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyBeginNode
removeallmethods
removeallclassmethods

set class RubyBeginNode class
category: '*maglev-ast'
method:
s_a: val
  | res |
  (res := self _basicNew) bodyNode: val .
  ^ res

%


set class RubyBeginNode
category: 'accessing'
method:
bodyNode

	 ^ bodyNode

%


set class RubyBeginNode
category: 'accessing'
method:
bodyNode: aNode
	bodyNode := aNode

%


set class RubyBeginNode
category: 'converting'
method:
irNode
	^ bodyNode irBeginBodyNode

%


set class RubyBeginNode
category: '*maglev-ast'
method:
kbegin_value
  ^ bodyNode

%


set class RubyBeginNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		nextPutAll: 'begin';
		indentAndEnd: bodyNode

%


set class RubyBeginNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  | loop |
  loop := RubyCompilerState current topLoop .
  loop ifNotNil:[  loop setHasBeginRescue ].
  bodyNode walkWithScope: aScope

%


set class RubyBeginNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:Begin , ' , bodyNode _inspect , $]

%

