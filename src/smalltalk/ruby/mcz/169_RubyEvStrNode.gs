
doit
RubyNode subclass: 'RubyEvStrNode'
	instVarNames: #( body)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyEvStrNode
removeallmethods
removeallclassmethods

set class RubyEvStrNode class
category: '*maglev-ast'
method:
s_a: body
 | res |
  (res := self _basicNew ) body: body .
  ^ res

%


set class RubyEvStrNode
category: 'accessing'
method:
body

	 ^ body

%


set class RubyEvStrNode
category: 'accessing'
method:
body: aNode
	body := aNode

%


set class RubyEvStrNode
category: '*maglev-ast'
method:
evStrBody
  ^ body

%


set class RubyEvStrNode
category: '*maglev-runtime'
method:
irNode
  body ifNil:[  ^ GsComLiteralNode newString:'' ]
       ifNotNil:[ :bdy |  ^ bdy irEvaluateBlock ]

%


set class RubyEvStrNode
category: 'printing'
method:
printSourceOn: aStream
	aStream printNode: body

%


set class RubyEvStrNode
category: '*maglev-ast'
method:
str_dstr_evstr_kind
  ^ 2

%


set class RubyEvStrNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
     body walkWithScope: aScope

%


set class RubyEvStrNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:evstr, ', body _inspect , $]

%

