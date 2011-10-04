
doit
RubyAbstractMatchDataRef subclass: 'RubyNthRefNode'
	instVarNames: #( matchNumNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyNthRefNode
removeallmethods
removeallclassmethods

set class RubyNthRefNode class
category: '*maglev-runtime'
method:
s_a: arg b: srcOfs
  | node rcvr | 
  arg _isSmallInteger ifFalse:[ self error:'nthRef arg must be a Fixnum'].
  (node := self _basicNew )
      matchNumber: arg .
  (rcvr := RubyVcGlobalNode _basicNew)  name: #'$~' .
  node rcvr: rcvr ; position: srcOfs .
  ^ node

%


set class RubyNthRefNode
category: '(as yet unclassified)'
method:
argNodes
  | s |
  ^ (s := matchNumNode ) ifNil:[ #() ] ifNotNil:[ { s } ]

%


set class RubyNthRefNode
category: '*maglev-ast'
method:
backRefErrorString
  ^ '$' + matchNumNode _value asString

%


set class RubyNthRefNode
category: 'as yet unclassified'
method:
isSmalltalkSend
  ^ true

%


set class RubyNthRefNode
category: 'as yet unclassified'
method:
matchNumber: anInt 
  matchNumNode := RubyFixnumNode newForInt: anInt

%


set class RubyNthRefNode
category: 'converting'
method:
selector
	^ #nthRegexRef:

%


set class RubyNthRefNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:nth_ref, ' , matchNumNode _inspect, $]

%

