
doit
RubyAbstractVarNode subclass: 'RubyGlobalVarNode'
	instVarNames: #( name globalAssoc)
	classVars: #( SpecialGlobalNodesDict)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalVarNode
removeallmethods
removeallclassmethods
