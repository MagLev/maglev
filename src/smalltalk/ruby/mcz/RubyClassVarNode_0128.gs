
set class RubyClassVarNode
category: 'as yet unclassified'
method:
definedQkind
  ^ #'class variable'

%


set class RubyClassVarNode
category: '*maglev-runtime'
method:
determineDynamic
   ^ 2

%


set class RubyClassVarNode
category: '*maglev-runtime'
method:
irDefinedQNode
  ^ self _irNodeWithSel:  #_rubyClassVarDefinedQ:mref:

%


set class RubyClassVarNode
category: '*maglev-runtime'
method:
irEvaluatedOpOrRcvr 
  ^ self _irNodeWithSel:  #_rubyClassVarGetOrNil:mref:

%


set class RubyClassVarNode
category: '*maglev-runtime'
method:
irNode
  ^ self _irNodeWithSel:  #_rubyClassVarGet:mref: 

%


set class RubyClassVarNode
category: 'parsetree-test'
method:
isSameAs: other
	^ self name = other name

%


set class RubyClassVarNode
category: 'accessing'
method:
name

	 ^ name

%


set class RubyClassVarNode
category: 'accessing'
method:
name: aString
	name := aString

%


set class RubyClassVarNode
category: '*maglev-runtime'
method:
walkWithScope: aScope 
  | cst |
  inMethod := RubyCompilerState current  topMethodDef isMethodDefinition .
  ^ super walkWithScope: aScope 

%


set class RubyClassVarNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:cvar, :', name  , $]

%


set class RubyClassVarNode
category: '*maglev-runtime'
method:
_irNodeWithSel:  runtimeSel  
  |  node  |
  (node := GsComSendNode new)
     rcvr:  GsComVariableNode newSelf ;
     stSelector: runtimeSel   ;
     appendArgument: (GsComLiteralNode newObject: name ) ;
     appendArgument: ( inMethod ifTrue:[ GsComLiteralNode newObject: 
                                          RubyCompilerState current topRtModule
                           ]
                           ifFalse:[  GsComVariableNode newSelf ] ) .
  ^ self ir: node

%

