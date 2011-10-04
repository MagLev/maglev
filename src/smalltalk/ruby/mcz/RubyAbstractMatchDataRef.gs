
doit
RubyAbstractCallNode subclass: 'RubyAbstractMatchDataRef'
	instVarNames: #( rcvrNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyAbstractMatchDataRef
removeallmethods
removeallclassmethods

set class RubyAbstractMatchDataRef
category: 'as yet unclassified'
method:
rcvr: aNode
  rcvrNode := aNode

%


set class RubyAbstractMatchDataRef
category: 'as yet unclassified'
method:
receiverNode
  "rcvrNode filled in by walkWithScope to be the class MatchData ,
   at runtime   selector   will resolve to a classmethod in MatchData  "

  ^ rcvrNode

%

