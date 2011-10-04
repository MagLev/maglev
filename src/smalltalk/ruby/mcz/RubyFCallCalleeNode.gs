
doit
RubyFCallNode subclass: 'RubyFCallCalleeNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyFCallCalleeNode
removeallmethods
removeallclassmethods

set class RubyFCallCalleeNode
category: '*maglev-runtime'
method:
irNode 
      "ruby_selector_suffix dependent"
  | snd |
  argsList size > 0 ifTrue:[ ^ super irNode ].
  snd := GsComSendNode new .
  RubyCompilerState current compilingEval ifTrue:[
    snd rcvr: (GsComLiteralNode newObject: GsProcess) ;
      stSelector: #_rubyEvalHomeMethod .
  ] ifFalse:[
    snd rcvr: (GsComLiteralNode newObject: Kernel);
      rubySelector: #'__method__#0__' .
  ].
  self ir: snd .
  ^ snd

%

