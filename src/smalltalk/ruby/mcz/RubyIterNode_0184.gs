
doit
RubyNode subclass: 'RubyIterNode'
	instVarNames: #( blockBody bodyNode scope
	                  varNode multArgsNode labelRedo labelNext
	                  zeroDeclaredArgs endSrcOfs)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyIterNode
removeallmethods
removeallclassmethods
