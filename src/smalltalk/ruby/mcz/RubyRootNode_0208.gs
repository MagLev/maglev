
doit
RubyNode subclass: 'RubyRootNode'
	instVarNames: #( bodyNode staticScope sendsBinding
	                  lineNumberBias isMainProgram fileName source)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyRootNode
removeallmethods
removeallclassmethods
