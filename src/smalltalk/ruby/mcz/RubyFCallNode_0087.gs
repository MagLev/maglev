
doit
RubyAbstractCallNode subclass: 'RubyFCallNode'
	instVarNames: #( argsNode callName iterNode
	                  rcvrNode implicitDollarTilde)
	classVars: #( SpecialRubySelectors)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyFCallNode
removeallmethods
removeallclassmethods
