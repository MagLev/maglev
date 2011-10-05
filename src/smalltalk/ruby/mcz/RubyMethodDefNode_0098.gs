
doit
RubyAbstractCallNode subclass: 'RubyMethodDefNode'
	instVarNames: #( argsNode bodyNode nameNode
	                  scope hasBlockArgRef irMethNode traceBool
	                  sendsBinding argsDescrInt methSelector lineBias
	                  fileName source startLine endOffset
	                  outerDef defTarget)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyMethodDefNode
removeallmethods
removeallclassmethods
