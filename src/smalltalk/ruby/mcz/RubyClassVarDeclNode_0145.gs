
set class RubyClassVarDeclNode class
category: 'Documentation'
method:
comment
  "8/1/2008 inherits nextNodeForParser: from RubyNode .
    may not be used or debugged yet."

%


set class RubyClassVarDeclNode
category: '*maglev-ast'
method:
as_accessor
  ^ RubyClassVarNode _basicNew name: name

%


set class RubyClassVarDeclNode
category: 'as yet unclassified'
method:
buildIrLeafsInto: anArray
 
                                   "see Trac 564, which is deferred for now"
  self signalParseError: ' class variable ' , name , ' not supported as an iterator name '
	

%


set class RubyClassVarDeclNode
category: '*maglev-runtime'
method:
irAssignmentNode: srcVarNode 
  ^ self _irNodeSrcIr: srcVarNode 

%


set class RubyClassVarDeclNode
category: '*maglev-runtime'
method:
irNode
  ^ self _irNodeSrcIr: valueNode irEvaluatedBlockNode 

%


set class RubyClassVarDeclNode
category: 'as yet unclassified'
method:
isSameAs: other
	^ (super isSameAs: other) and: [self name = other name]

%


set class RubyClassVarDeclNode
category: 'accessing'
method:
name

	 ^ name

%


set class RubyClassVarDeclNode
category: 'accessing'
method:
name: aString
	name := aString

%


set class RubyClassVarDeclNode
category: '*maglev-runtime'
method:
setIsBlockArg
  ^ self signalParseError: 'class variable not supported as a block argument'

%


set class RubyClassVarDeclNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  inMethod := RubyCompilerState current topMethodDef isMethodDefinition .
  super walkWithScope: aScope 

%


set class RubyClassVarDeclNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:cvasgn, :', name, ', ', valueNode _inspect , $]

%


set class RubyClassVarDeclNode
category: '*maglev-runtime'
method:
_irNodeSrcIr: srcIr 
  |  node |
  (node := GsComSendNode new)
     rcvr:  GsComVariableNode newSelf ;
     stSelector:  #addRubyClassVar:value:mref:   ;
     appendArgument: (GsComLiteralNode newObject: name ) ;
     appendArgument:  srcIr  ;
     appendArgument: (inMethod ifTrue:[ GsComLiteralNode newObject: 
                                          RubyCompilerState current topRtModule
                                   ]
                                    ifFalse:[ GsComVariableNode newSelf ] ) .
  ^ self ir: node 

%

