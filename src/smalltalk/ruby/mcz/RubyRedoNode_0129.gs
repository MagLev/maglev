
doit
RubyAbstractGotoNode subclass: 'RubyRedoNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyRedoNode
removeallmethods
removeallclassmethods

set class RubyRedoNode
category: '*maglev-runtime'
method:
irNode
  | aLoop node cmState  |
  aLoop := (cmState := RubyCompilerState current) topLoop .
  aLoop ifNil:[
    (node := GsComSendNode new)
       rcvr: (GsComLiteralNode newObject: CannotReturn) ;
       stSelector:  #signal:  ; 
       appendArgument: (GsComLiteralNode newString: 'unexpected redo' ).
  ] ifNotNil:[ |targLev targ |
    targLev := (targ := aLoop labelRedo)  lexLevel .
    (cmState lexLevelIsInlineWithin: targLev) ifTrue:[ 
       (node := GsComGotoNode new ) localRubyRedo: targ 
    ] ifFalse:[
      (node := GsComSendNode new)
        rcvr: GsComLiteralNode newNil ;
        stSelector: #_rubyNext:with: ;
        appendArgument: (GsComLiteralNode newInteger: 1 );
        appendArgument: GsComLiteralNode newNil .
    ].
  ].
  self ir: node  .
  ^ node 

%


set class RubyRedoNode
category: 'as yet unclassified'
method:
nameForPrint
  ^ 'redo'

%


set class RubyRedoNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:redo ]'

%

