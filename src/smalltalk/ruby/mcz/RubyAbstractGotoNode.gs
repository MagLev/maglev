
doit
RubyNode subclass: 'RubyAbstractGotoNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyAbstractGotoNode
removeallmethods
removeallclassmethods

set class RubyAbstractGotoNode class
category: '*maglev-ast'
method:
s_a: srcOfs
  "used for RubyRedoNode, RubyRetryNode"
  | res |
  (res := self _basicNew) position: srcOfs .
  ^ res

%

