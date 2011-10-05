
doit
Object subclass: 'RubyPersistableCompilerState'
	instVarNames: #( envId fileStack scopeStack
	                  lexLevel lexLevelStack compilerStack methStack
	                  lineBiasStack loopStack lastExceptionStack outerDefLexPath
	                  installingPrims persistenceMode persistableInstances parserStack
	                  evalLexicalSelfStack rtModuleStack reloadingPrims)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyPersistableCompilerState
removeallmethods
removeallclassmethods
