
doit
RubyAbstractBreakNode subclass: 'RubyBreakNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyBreakNode
removeallmethods
removeallclassmethods

set class RubyBreakNode
category: 'as yet unclassified'
method:
childrenForMatch
	^ {valueNode}

%


set class RubyBreakNode
category: 'as yet unclassified'
method:
irGotoNodeFrom: aLoop to: aLabel
  | node |
  (node := GsComGotoNode new)  "goto within the same in-line loop"
     localRubyBreak: aLabel  .
   ^ node

%


set class RubyBreakNode
category: 'as yet unclassified'
method:
irGotoTarget: aLoop
  
  aLoop ifNil:[ ^ nil ].
  ^ aLoop labelBreak

%


set class RubyBreakNode
category: 'as yet unclassified'
method:
nameForPrint
  ^ 'break'

%


set class RubyBreakNode
category: '*maglev-runtime'
method:
nonInlineIrNode
   | node  |            
   (node := GsComSendNode new)
      rcvr: (GsComLiteralNode newObject: RubyBreakException ) ;
      stSelector:  #signalBreakWith:  ;
      appendArgument: self irGotoValueNode .
   self ir: node .
   ^ node .

%


set class RubyBreakNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:break, ', valueNode _inspect , $]

%

