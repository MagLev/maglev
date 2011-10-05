
doit
RubyAbstractCallNode subclass: 'RubyCallNode'
	instVarNames: #( argsNode iterNode receiverNode
	                  evaluationTmpAssoc procNewZeroHasMeth implicitDollarTilde callName)
	classVars: #( SpecialRubySelectors)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyCallNode
removeallmethods
removeallclassmethods
