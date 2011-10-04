
doit
RubyAbstractBreakNode subclass: 'RubyNextNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyNextNode
removeallmethods
removeallclassmethods

set class RubyNextNode
category: 'as yet unclassified'
method:
irGotoNodeFrom: aLoop to: aLabel
  | node |
  (node := GsComGotoNode new)  "goto within the same in-line loop"
     localRubyNext: aLabel argForValue: aLabel argForValue .
   ^ node

%


set class RubyNextNode
category: 'as yet unclassified'
method:
irGotoTarget: aLoop
  
  aLoop ifNil:[ ^ nil ].
  ^ aLoop labelNext

%


set class RubyNextNode
category: 'as yet unclassified'
method:
nameForPrint
  ^ 'next'

%


set class RubyNextNode
category: '*maglev-runtime'
method:
nonInlineIrNode
   "next in a non-inline loop does a normal return from block "
  | node | 
  node := GsComSendNode new.
  (RubyCompilerState current topLoop) ifNil:[  "fix 861"
    node rcvr: (GsComLiteralNode newObject: CannotReturn) ;
      stSelector:  #signal:  ; 
      appendArgument: (GsComLiteralNode newString: 'unexpected next' ).
  ] ifNotNil:[
    node rcvr: self irGotoValueNode;
      stSelector: #_rubyNext:with: ;
      appendArgument: (GsComLiteralNode newInteger: 0 );
      appendArgument: GsComLiteralNode newNil .
  ].
  self ir: node .
  ^ node 

%


set class RubyNextNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:next, ', valueNode _inspect , $]

%

