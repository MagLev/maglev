
doit
RubyAbstractLiteralNode subclass: 'RubyEvalLexPathNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyEvalLexPathNode
removeallmethods
removeallclassmethods

set class RubyEvalLexPathNode
category: '*maglev-runtime'
method:
irNode 
 | snd |
  (snd := GsComSendNode new)
    rcvr: (GsComLiteralNode newObject: RubyCompilerState current rtModuleEvalLexPath) ;
    stSelector: #shallowCopy  .
  ^ self ir: snd

%

