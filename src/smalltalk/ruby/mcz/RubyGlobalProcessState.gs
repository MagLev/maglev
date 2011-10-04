
doit
RubyGlobalVarNode subclass: 'RubyGlobalProcessState'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalProcessState
removeallmethods
removeallclassmethods

set class RubyGlobalProcessState
category: 'as yet unclassified'
method:
irNode
  | node |
  node := GsComSendNode new.
  name == #'$?' ifTrue:[
	node rcvr: ( GsComLiteralNode newObject: GsProcess );
       stSelector: #_rubyThreadDataAt:  ;
       appendArgument: (GsComLiteralNode newInteger: 2) .
  ] ifFalse:[
     self error: 'invalid RubyGlobalProcess access'
  ].
  self ir: node .
  ^ node

%

