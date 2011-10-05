
doit
RubyAbstractGotoNode subclass: 'RubyRetryNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyRetryNode
removeallmethods
removeallclassmethods

set class RubyRetryNode
category: '*maglev-runtime'
method:
irNode
   | aLeaf node |
   node := GsComSendNode new .
   aLeaf := RubyCompilerState current lastExceptionStack topOrNil .
   aLeaf ifNotNil:[ 
     "restart execution after nearest preceeding 'begin'  keyword,
      equivalent to resume execution of receiver of on:do:  "
     node rcvr: (GsComVariableNode new leaf: aLeaf) ;
       stSelector:  #retry  .  
   ] ifNil:[
     node rcvr: (GsComLiteralNode newObject: CannotReturn ) ; "ruby LocalJumpError"
        stSelector: #signal:   ;
       appendArgument: ( GsComLiteralNode newString: 'retry outside of rescue clause').
   ].
   ^ self ir: node

%


set class RubyRetryNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:retry ]'

%

