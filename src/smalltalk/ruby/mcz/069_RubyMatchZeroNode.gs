
doit
RubyAbstractMatchNode subclass: 'RubyMatchZeroNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyMatchZeroNode
removeallmethods
removeallclassmethods

set class RubyMatchZeroNode class
category: '*maglev-ast'
method:
s_a: aRegexNode
  | node  str |
  ( str := RubyVcGlobalNode _basicNew) name: #'$_' .
  (node := RubyMatch2Node _basicNew) 
     receiverNode: str ;
     valueNode:  aRegexNode .
  node initImplicitDollarTildeForRp .
  ^ node 

%


set class RubyMatchZeroNode
category: 'as yet unclassified'
method:
irNode
  self error:'should not be here'

%


set class RubyMatchZeroNode
category: '*maglev-runtime'
method:
_inspect
  Error signal:' expect no instances of RubyMatchZeroNode'

%

