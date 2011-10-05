
doit
RubyAbstractCallNode subclass: 'RubyVCallNode'
	instVarNames: #( callName rcvrNode implicitDollarTilde)
	classVars: #( SpecialRubySelectors)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyVCallNode
removeallmethods
removeallclassmethods
