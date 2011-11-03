
set class RubyLocalStaticScope
category: '*maglev-runtime'
method:
addImplicitBlockTemp

	hasImplicitBlockTemp ifNil: [
	  self addVariableNames: { self implicitBlockTempName }.
	  hasImplicitBlockTemp := true]

%


set class RubyLocalStaticScope
category: '*maglev-runtime'
method:
addIncomingBlock: nameSym
  | vInfo |
  vInfo := self _findVariable: nameSym .
  vInfo ifNotNil:[ self error:'incomingBlockArg already defined'].
  vInfo := self _addVarInfo: RubyScopeIncomingBlockVar name: nameSym kind: #incomingBlockArg .
  vInfo toProcInfo: self newEvaluationTemp boot: inBootstrap .
 
  ^ RubyVarLocation _basicNew varInfo: vInfo depth: 0 scope: self .

%


set class RubyLocalStaticScope
category: '*maglev-runtime'
method:
blockArgLeaf: aVarLeaf
  | leaf |
  (leaf := aVarLeaf) ifNil:[
     ( leaf := incBlockArgLeaf) ifNil:[ 
       leaf := GsComVarLeaf new methodArg: self implicitBlockTempName argNumber: self numArgs + 1 .
     ].
  ].
  incBlockArgLeaf := leaf .
  ^ leaf 

%


set class RubyLocalStaticScope
category: '*maglev-runtime'
method:
implicitBlockNotNil
  | leaf |
  (leaf := incBlockArgLeaf) ifNil:[ ^ nil ].
  ^ hasImplicitBlockTemp == true
	ifTrue: [GsComVariableNode new leaf: (self _findVariable: self implicitBlockTempName) leaf]
	ifFalse: [GsComVariableNode new leaf: leaf].

%


set class RubyLocalStaticScope
category: '*maglev-runtime'
method:
implicitBlockTempName

	^ #'_block'

%


set class RubyLocalStaticScope
category: '*maglev-runtime'
method:
implicitBlockVar
  
  ^ self implicitBlockNotNil ifNil: [ ^ GsComLiteralNode newNil ].

%


set class RubyLocalStaticScope
category: '*maglev-runtime'
method:
lexLevel
	^ 0

%


set class RubyLocalStaticScope
category: '*maglev-runtime'
method:
methodScope
  ^ self 

%


set class RubyLocalStaticScope
category: '*maglev-runtime'
method:
newArgLeafNamed: aSym number: aNumber
  | res  | 
  (res := GsComVarLeaf new)
			methodArg: aSym 
			argNumber: aNumber .
  TraceLocals >= 1 ifTrue:[
	self trace: ' argLeafAtIndex:', aNumber asString , ' created varLeaf methArg for ', aSym
  ].
  ^ res 

%


set class RubyLocalStaticScope
category: '*maglev-runtime'
method:
newTempLeafNamed: aSymbol
  | theLeaf |
  theLeaf :=  GsComVarLeaf new .
  (aSymbol == #'$~' or:[ aSymbol == #'$_' or:[ aSymbol == #'__lexPath']]) 
     ifTrue:[   theLeaf methVcGlobal: aSymbol ] 
    ifFalse:[  theLeaf methodTemp: aSymbol ].

  TraceLocals >= 1 ifTrue:[
    self trace: ' tempLeafNamed: ', aSymbol , ' , created methTemp leaf'
  ].
  ^  theLeaf

%


set class RubyLocalStaticScope
category: '*maglev-runtime'
method:
setGlobalScopeFor: nameNode parent: aLocalScope env: envId 
  "nameNode should be a ClassNameNode"
  | parentGscop ns baseName c2path |
  variableNames := { } . "inline clearVariableNames" 
  inBootstrap := aLocalScope inBootstrap .
  nameNode isColon3 ifTrue:[
     parentGscop := Object 
  ] ifFalse:[  
    parentGscop := aLocalScope nameSpace .
    c2path := nameNode pathArray . 
    c2path size > 1 ifTrue:[   ns := parentGscop scopeAtPath: c2path env: envId ].
  ].            
  ns ifNil:[
    baseName := nameNode  baseName asSymbol .   
    ns := parentGscop ifNotNil:[ parentGscop childScopeAt: baseName isDefine: true env: envId ] .
  ].
  nameSpace := ns 

%

