
doit
RubyAbstractScope subclass: 'RubyStaticScope'
	instVarNames: #( isArgumentScope requiredArgs restArg
	                  enclosingScope variableNames extraArgs inBootstrap
	                  nameSpace)
	classVars: #( TraceLocals)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyStaticScope
removeallmethods
removeallclassmethods
