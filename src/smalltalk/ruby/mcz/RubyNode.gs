
set class RubyNode
category: '(as yet unclassified)'
classmethod:
new
  self error:'should use newFor: '

%


set class RubyNode
category: 'as yet unclassified'
method:
adjustForLhsSplat
  ^ self

%


set class RubyNode
category: 'as yet unclassified'
method:
allChildren
	^ self childrenForMatch select: [:ea | ea isKindOf: RubyNode]

%


set class RubyNode
category: 'as yet unclassified'
method:
argNodes
	^ {self}

%


set class RubyNode
category: 'as yet unclassified'
method:
asRpMasgnList
  ^ nil

%


set class RubyNode
category: '*maglev-ast'
method:
as_cond
  ^ self

%


set class RubyNode
category: 'as yet unclassified'
method:
buildConstRef
  ^ nil

%


set class RubyNode
category: '*maglev-runtime'
method:
buildIrMethodNode: aBlock
  | cmState node |
  (cmState := RubyCompilerState current) pushMethodDef: self .
  (node := GsComMethNode newRuby)    
     environment: cmState envId ;
     setRubyLineNumberBias:  self lineBias ;
     fileName: self fileName source: self source .
  [
    aBlock value: node  .
  ] ensure:[
    cmState popMethodDef: self 
  ].
   self ir: node .
   ^ node

%


set class RubyNode
category: '*maglev-runtime'
method:
buildStatementsOn: irNode
    | nl last rtn  |
    self irNodeListInto: irNode .
    irNode lastAsRubyReturn .

%


set class RubyNode
category: 'as yet unclassified'
method:
compiler
	^ RubyCompilerState current topCompiler

%


set class RubyNode
category: 'as yet unclassified'
method:
currentScope
	^ RubyCompilerState current topScope

%


set class RubyNode
category: 'as yet unclassified'
method:
definedQkind
  "Returns a string constant which will be the result of a   defined?   operator.
   See Pickaxe page 94 .   defined?  is a reserved word operator, not a message send."

  ^ #expression

%


set class RubyNode
category: '*maglev-ast'
method:
definedQNode
  ^ RubyStrNode newForIr _value:  self  definedQkind asString

%


set class RubyNode
category: 'parsetree-as yet unclassified'
method:
detectMismatchWith: other
	self species = other species ifFalse: [^ {self. other}].
	(self isSameAs: other) ifFalse: [^ {self. other}].
	self childrenForMatch 
		with: other childrenForMatch 
		do: [:a :b |
		       a == b ifFalse:
			[(a detectMismatchWith: b) ifNotNilDo: [:c | ^ c]]].
	^ nil

%


set class RubyNode
category: '*maglev-runtime'
method:
doesNotUnderstand: aSymbol args: anArray envId: envId
  aSymbol == #lineForOffset: ifTrue:[
    ArgumentError signal:'lineForOffset not implemented.'
  ] .
  ^ self error:'a' , self class name , ' doesNotUnderstand  ', aSymbol 

%


set class RubyNode
category: '*maglev-runtime'
method:
error: msg 
  | fullMsg |
  (fullMsg := msg copy )
    add: ' , near ' ;
     add: self sourcePositionAsString .
  ^ Error signal: fullMsg 

%


set class RubyNode
category: 'parsetree-as yet unclassified'
method:
establishScope

%


set class RubyNode
category: 'as yet unclassified'
method:
fCallArgNodes
  ^ self argNodes

%


set class RubyNode
category: '*maglev-runtime'
method:
hasInnerDefs
  ^ false

%


set class RubyNode
category: 'as yet unclassified'
method:
hasRestArg
	^ false

%


set class RubyNode
category: '*maglev-runtime'
method:
innerDefs
  ^ 0

%


set class RubyNode
category: 'as yet unclassified'
method:
installingPrims

  ^ RubyCompilerState current installingPrims

%


set class RubyNode
category: '*maglev-runtime'
method:
instvarAccessKindFor: aSymbol
  "Return 0 if aSymbol is an instVar with a fixed offset or can use a dynamicIv bytecode.
        1 if it is a dynamic instVar in an instance of Object or a kind of Behavior"

 | comp cls trc  |
  "trc := UserGlobals at:#IvTrace ."
  comp := self compiler .
  (comp instVarExists: aSymbol with: self) ifFalse:[
    cls := comp currentClass .
    (cls == Object or:[ cls isMetaOrModule]) ifTrue:[
       " trc addLast: { aSymbol . cls .  1} ."
      ^ 1
    ].
  ].
  "trc addLast: { aSymbol . cls . 0}."
  ^ 0

%


set class RubyNode
category: 'as yet unclassified'
method:
ir: aNode
  "install source position info into the receiver."
    | pos  |
	(pos := position) _isSmallInteger ifTrue:[ aNode sourceOffset: pos  "byte offset from RubyParser" ]
	              ifFalse:[  pos ifNotNil:[  pos storePositionInNode: aNode  "line number from MRI server"]].
	^ aNode

%


set class RubyNode
category: 'as yet unclassified'
method:
irArgNode
  ^ self irNode

%


set class RubyNode
category: 'as yet unclassified'
method:
irBeginBodyNode
  ^ self irNode

%


set class RubyNode
category: '*maglev-runtime'
method:
irBlockNode: parentNode
    "parentNode not used here"
    ^ self newBlock:
       [:block | | list sz  |
          self irNodeListInto: block .
          block 
       ]

%


set class RubyNode
category: '*maglev-runtime'
method:
irBlockNodeInline: parentNode
   "parentNode not used if receiver is a RubyNode"
    ^ self newInlineBlock:
       [:block | | list |
         self irNodeListInto: block .
         block
       ]

%


set class RubyNode
category: '*maglev-runtime'
method:
irBlockPassNode
      "ruby_selector_suffix dependent"
  | send |  
  ( send := GsComSendNode new)
     rcvr: self irNode ;
     rubySelector: #'__to_proc_arg#0__' .
  self ir: send.
  ^ send 

%


set class RubyNode
category: '*maglev-runtime'
method:
irCaseNodeListWithLeaf: aLeaf into: blockIr
    ^ self irNodeListInto: blockIr

%


set class RubyNode
category: 'as yet unclassified'
method:
irCaseNodeWithNode: aNode
	^ self irNode

%


set class RubyNode
category: '*maglev-runtime'
method:
irCompilerNode
   "sender responsible for installing source position in result"   
    ^ GsComSendNode new
        rcvr: ( GsComLiteralNode newObject:  RubyCompiler ) ;
        stSelector: #new 

%


set class RubyNode
category: 'as yet unclassified'
method:
irDefinedQNode
  ^ self definedQNode irNode 

%


set class RubyNode
category: '*maglev-runtime'
method:
irEvaluateBlock
  ^ self irNode

%


set class RubyNode
category: 'as yet unclassified'
method:
irEvaluatedBlockNode
  ^ self irNode

%


set class RubyNode
category: 'as yet unclassified'
method:
irEvaluatedOpOrRcvr
   ^ self  irEvaluatedRcvrNode

%


set class RubyNode
category: 'as yet unclassified'
method:
irEvaluatedRcvrNode
  ^ self irNode

%


set class RubyNode
category: '*maglev-runtime'
method:
irForNode
      "ruby_selector_suffix dependent"
  | node |
  node := (GsComSendNode new)
            rcvr: self irNode;
            rubySelector: #'__each#0_&' .
  self ir: node .
  ^ node 

%


set class RubyNode
category: 'as yet unclassified'
method:
irForNodeWillBeInline
  ^ false

%


set class RubyNode
category: 'as yet unclassified'
method:
irInlineBlockIr: anIrNode 
    | block |
	block := GsComBlockNode new.
	self nextLexLevelInline:
		[:level |
		 block lexLevel: level;
		   appendStatement: anIrNode .
		].
    ^  block

%


set class RubyNode
category: 'as yet unclassified'
method:
irLitVarLeaf: anAssoc
  | val assoc |
  assoc := anAssoc .
  (assoc isKindOf: RubySymbolAssociation) ifFalse:[ self error:'invalid association'].
  ^ GsComVarLeaf new literalVariable:  assoc

%


set class RubyNode
category: 'as yet unclassified'
method:
irLocalAsgnValue
  ^ self irEvaluatedBlockNode

%


set class RubyNode
category: 'as yet unclassified'
method:
irNode
	self error: 'irNode not yet implemented for ', self class name

%


set class RubyNode
category: '*maglev-runtime'
method:
irNodeListInto: blockIr

  blockIr appendStatement: self irNode

%


set class RubyNode
category: '*maglev-runtime'
method:
irNonSplatNode
  
  ^ self irNode

%


set class RubyNode
category: 'as yet unclassified'
method:
irReturnNode
  ^ self irNode

%


set class RubyNode
category: '*maglev-runtime'
method:
irWhenExpressionWithLeaf: aLeaf
      "ruby_selector_suffix dependent"
  | node |
  aLeaf ifNotNil:[
    (node := GsComSendNode new)
        rcvr:  self irNode ;
        rubySelector:  #'===#1__'  ;
        appendArgument: (GsComVariableNode new leaf: aLeaf) .
    self ir: node .
    ^ node 
  ] ifNil:[  "parsing   case with no target expression" 
     ^ self irNode 
  ].

%


set class RubyNode
category: 'as yet unclassified'
method:
isArgsCatNode
  ^ false

%


set class RubyNode
category: 'parsetree'
method:
isBlockArgNode
	^ false

%


set class RubyNode
category: 'as yet unclassified'
method:
isBlockPassNode
  ^ false

%


set class RubyNode
category: 'as yet unclassified'
method:
isEmptySplatNode
  ^ false

%


set class RubyNode
category: '*maglev-runtime'
method:
isInnerDef
  ^ false

%


set class RubyNode
category: '*maglev-runtime'
method:
isMethodDef
  ^ false

%


set class RubyNode
category: 'as yet unclassified'
method:
isMethodDefinition
	^ false

%


set class RubyNode
category: '*maglev-runtime'
method:
isModuleClass
  ^ false

%


set class RubyNode
category: 'as yet unclassified'
method:
isNilNode
  ^ false

%


set class RubyNode
category: 'as yet unclassified'
method:
isParAssignNode
  ^ false

%


set class RubyNode
category: 'as yet unclassified'
method:
isProcClass
  ^ false

%


set class RubyNode
category: 'as yet unclassified'
method:
isRescueBodyNode
  ^ false

%


set class RubyNode
category: 'parsetree-test'
method:
isSameAs: other
	^ true

%


set class RubyNode
category: 'as yet unclassified'
method:
isSplatNode
  ^ false

%


set class RubyNode
category: 'as yet unclassified'
method:
isToAryNode
  ^ false

%


set class RubyNode
category: 'as yet unclassified'
method:
isTypeClass
  ^ false

%


set class RubyNode
category: '*maglev-ast'
method:
is_void_result
  ^ false

%


set class RubyNode
category: '*maglev-ast'
method:
kbegin_value
  ^ self

%


set class RubyNode
category: 'as yet unclassified'
method:
list
	^ {  self }

%


set class RubyNode
category: 'parsetree-test'
method:
matches: other
	^ (self detectMismatchWith: other) isNil

%


set class RubyNode
category: '*maglev-runtime'
method:
name
  ^ self error:'should not implement name'

%


set class RubyNode
category: 'as yet unclassified'
method:
newBlock: aBlock
	|block|
	block := GsComBlockNode new.
	self ir: block . "to help computeLastLineNumber, Trac 708"
	^ self nextLexLevel:
		[:level |
		block lexLevel: level.
		aBlock value: block]

%


set class RubyNode
category: 'as yet unclassified'
method:
newBlock: aBlock isInline: inlineBool
^  inlineBool ifTrue:[ self newInlineBlock: aBlock ]
	        ifFalse:[ self newBlock: aBlock ]

%


set class RubyNode
category: 'as yet unclassified'
method:
newInlineBlock: aBlock
	| block |
	block := GsComBlockNode new.
	^ self nextLexLevelInline:
		[:level |
		block lexLevel: level.
		aBlock value: block]

%


set class RubyNode
category: 'as yet unclassified'
method:
nextLexLevel: aBlock
	| level cmState res  |
	cmState := RubyCompilerState current .
	level := cmState pushLexicalLevel .
	[ 
	  res := aBlock value: level
	] ensure:[
	  cmState popLexicalLevel: level 
	].
     ^ res

%


set class RubyNode
category: 'as yet unclassified'
method:
nextLexLevelInline: aBlock
	| level cmState res  |
	cmState := RubyCompilerState current .
	level := cmState pushInlineLexicalLevel .
	[ 
	  res := aBlock value: level
	] ensure:[
	  cmState popLexicalLevel: level 
	].
     ^ res

%


set class RubyNode
category: '*maglev-ast'
method:
node_assign_set_rhs: rhs
  RubyParserM signalError: 'invalid lhs for node_assign' .
  ^ nil

%


set class RubyNode
category: 'as yet unclassified'
method:
parAsgnToarySelector
  ^ #__par_asgn_to_ary

%


set class RubyNode
category: 'accessing'
method:
position

	 ^ position

%


set class RubyNode
category: '*maglev-runtime'
method:
position: pos
  pos _isSmallInteger ifFalse:[   "TODO remove check"
     nil pause .
     RubyParserM signalError: 'invalid source position'
  ].
  position := pos

%


set class RubyNode
category: '*maglev-runtime'
method:
postWalkForYield
  ^ self 

%


set class RubyNode
category: '*maglev-ast'
method:
prepend_to_block: aNode
 ^ RubyBlockNode s_list: { aNode . self }

%


set class RubyNode
category: 'as yet unclassified'
method:
printSourceOn: aStream
	aStream nextPutAll: self class name asUppercase

%


set class RubyNode
category: 'as yet unclassified'
method:
rpCallArgsList
   ^ #()

%


set class RubyNode
category: 'as yet unclassified'
method:
setForLoopNotInline
   ^ self " do nothing"

%


set class RubyNode
category: '*maglev-runtime'
method:
setHasInnerDefs
  "do nothing"

%


set class RubyNode
category: '*maglev-runtime'
method:
setHasInnerEvalOrDef
  ^ self

%


set class RubyNode
category: '*maglev-runtime'
method:
setIsInnerDef: outer
  "do nothing"

%


set class RubyNode
category: '*maglev-ast'
method:
setParen
  "called from grammar.y via ast.h"
  "do nothing"
  ^ self

%


set class RubyNode
category: '(as yet unclassified)'
method:
signal: exceptionClass with: msg
  | fullMsg |
  (fullMsg := msg copy )
    add: ' , near ' ;
     add: self sourcePositionAsString .
  ^ exceptionClass signal: fullMsg

%


set class RubyNode
category: 'as yet unclassified'
method:
signalParseError: aString
  ^ self signalParseError: aString node: self

%


set class RubyNode
category: 'as yet unclassified'
method:
signalParseError: aString node: aNode
  RubyParseError signal: aString , ', near ',  aNode sourcePositionAsString  . 
  ^ nil

%


set class RubyNode
category: 'as yet unclassified'
method:
sourcePositionAsShortString
  | pos |
  (pos := position) ifNil:[ ^ 'unknown source position'].
  pos _isInteger ifTrue:[
	 ^ self sourcePositionAsString .  "RubyParser"
  ].
  ^ position asShortString "SimpleSourcePosition from MRI parse server"

%


set class RubyNode
category: '*maglev-runtime'
method:
sourcePositionAsString
  | pos |
  (pos := position) ifNil:[ ^ 'unknown source position'].
  pos _isInteger ifTrue:[ |  cst mth  | 
    cst := RubyCompilerState current .
   "( parser := cst topRubyParserOrNil) ifNotNil:[
      ^ 'line ' ,  ( parser with: pos perform: #line_for_offset: env:2 ) asString  
    ].
   "
   (mth := cst topMethodDefOrNil) ifNotNil:[  "use implementation in RubyRootNode"
     ^ [ 'line ' , (mth lineForOffset: pos) asString  
       ] on: ArgumentError do:[:ex |
         ^ 'byte ' , mth position asString
       ]
    ]
  ].
   ^ position asString  "SimpleSourcePosition from MRI parse server"

%


set class RubyNode
category: 'as yet unclassified'
method:
sourceString
	| stream |
	stream := IndentedStream on: String new.
	self printSourceOn: stream.
	^ stream contents

%


set class RubyNode
category: '*maglev-ast'
method:
str_dstr_evstr_kind
  ^ nil

%


set class RubyNode
category: 'as yet unclassified'
method:
useScope: aScope during: aBlock
    
	^ self nextLexLevel: [:i |  | scopeStk res |
		scopeStk := RubyCompilerState current scopeStack .
		scopeStk push: aScope .
		[
		  res:= aBlock value 
		] ensure: [
		   scopeStk pop: aScope
		].
	     res
	  ]
		

%


set class RubyNode
category: '(as yet unclassified)'
method:
walk: childrenList withScope: aScope
  1 to: childrenList size do:[:j| | ea |
    (ea := childrenList at: j) ~~ nil ifTrue:[ ea walkWithScope: aScope ]
  ].

%


set class RubyNode
category: 'as yet unclassified'
method:
walkForDefinedQ: aScope

  ^ self walkWithScope: aScope

%


set class RubyNode
category: '*maglev-ast'
method:
walkIterRpVar
  ^ self

%


set class RubyNode
category: 'as yet unclassified'
method:
walkIterWithScope: aScope
  ^ self walkWithScope: aScope

%


set class RubyNode
category: 'as yet unclassified'
method:
walkRpList: list 
  "used by ParAsgnRpNode>>walkRpNode"
  1 to: list size do:[:n |
	(list at: n) walkRpNode
  ].

%


set class RubyNode
category: 'as yet unclassified'
method:
walkRpNode
  ^ self

%


set class RubyNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
    ^ self

%


set class RubyNode
category: 'as yet unclassified'
method:
walkWithScope: aScope isDefinedQ: isQ 

  ^ isQ ifTrue:[ self walkForDefinedQ: aScope ] 
        ifFalse:[ self walkWithScope: aScope]

%


set class RubyNode
category: 'as yet unclassified'
method:
yieldArgsList
   ^ self list

%


set class RubyNode
category: '*maglev-runtime'
method:
_inspect
  Error signal:'missing implementation of _inspect for ' , self class name

%

