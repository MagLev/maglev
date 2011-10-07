
set class RubySClassNode
category: '*maglev-runtime'
classmethod:
s_a: rcv b: body c: srcOfs d: lineNum
  | res |
  (res := self _basicNew)
    receiverNode: rcv ; 
    bodyNode: body ;
    lineBias: 0   ;
    position: srcOfs ; startLine: lineNum .
  ^ res

%


set class RubySClassNode
category: 'accessing'
method:
bodyNode

	 ^ bodyNode

%


set class RubySClassNode
category: '*maglev-runtime'
method:
bodyNode: aName
    bodyNode := aName 

%


set class RubySClassNode
category: '(as yet unclassified)'
method:
comIrMethNode
   ^ nil  "not a method definition"

%


set class RubySClassNode
category: 'as yet unclassified'
method:
fileName
  ^ fileName

%


set class RubySClassNode
category: 'as yet unclassified'
method:
hasSource
  ^ false  "does not hold a sourceString"

%


set class RubySClassNode
category: '*maglev-runtime'
method:
irArgNodes
   | cst |
   cst := RubyCompilerState current .
   ^ { receiverNode irNode . 
         self literalNode . 
         cst topScope  implicitBlockVar .
         GsComLiteralNode newObject: 
            (Array withAll:(cst rtModuleStack ))
     } 

%


set class RubySClassNode
category: '*maglev-runtime'
method:
irMethodNode: envId forClass: aClass 

  | irMeth |
  irMeth := self buildIrMethodNode: [ :node |  
      node forceAllArgsTmpsToVc . "patch for Trac678"
      self useScope: scope during: [  | blkarg | "always implicit block, Trac808"
        scope buildTempsOn: node.
        node appendArg: (blkarg := scope blockArgLeaf: nil ). 
        bodyNode buildStatementsOn: node .
      ].
   ].
   irMeth sourceOffset: position .
   ^ irMeth

%


set class RubySClassNode
category: 'accessing'
method:
irReceiverNode
	^ self ir:( self irCompilerNode )

%


set class RubySClassNode
category: 'accessing'
method:
isSmalltalkSend
	^ true

%


set class RubySClassNode
category: 'as yet unclassified'
method:
lineBias
 ^ lineBias

%


set class RubySClassNode
category: 'as yet unclassified'
method:
lineBias: anInt 
  lineBias := anInt

%


set class RubySClassNode
category: '*maglev-runtime'
method:
lineForOffset: anOffset
  ^ startLine "ignore anOffset"

%


set class RubySClassNode
category: 'accessing'
method:
literalNode
	^ self ir: (GsComLiteralNode new leaf: (self ir: (GsComLitLeaf new methodLiteral: self)))

%


set class RubySClassNode
category: 'accessing'
method:
receiverNode

	 ^ receiverNode

%


set class RubySClassNode
category: 'accessing'
method:
receiverNode:  aNode
	receiverNode := aNode

%


set class RubySClassNode
category: 'accessing'
method:
scope

	 ^ scope

%


set class RubySClassNode
category: '*maglev-runtime'
method:
scope: scp
    scope := scp

%


set class RubySClassNode
category: '*maglev-runtime'
method:
selector
    ^ #extend:rubyMethod:blk:rtModulePath:

%


set class RubySClassNode
category: 'as yet unclassified'
method:
selectorPrefix
  ^ ''  "not a method def"

%


set class RubySClassNode
category: '*maglev-runtime'
method:
setHasBlockArgRef
  | mth |
   mth := RubyCompilerState current parentMethodDefOrNil .
  (mth ~~ nil and:[ mth ~~ self]) ifTrue:[ ^ mth  setHasBlockArgRef ].
  ^ false

%


set class RubySClassNode
category: 'as yet unclassified'
method:
setSendsBinding
  ^ self

%


set class RubySClassNode
category: 'as yet unclassified'
method:
source
  ^ source

%


set class RubySClassNode
category: '*maglev-runtime'
method:
startLine: aLine
  startLine := aLine

%


set class RubySClassNode
category: '*maglev-runtime'
method:
walkInEval
  | mth |
   mth := RubyCompilerState current parentMethodDefOrNil .
  (mth ~~ nil and:[ mth ~~ self]) ifTrue:[ ^ mth  walkInEval ].
  ^ false

%


set class RubySClassNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
    "used for  class defns of forms      class << anObj  ,   class << ACls  "
  | newScop bdy cst file parentMth |  
  receiverNode walkWithScope: aScope .
  newScop := (scope := RubyLocalStaticScope new ).
  newScop 
    nonInheritingChildOf: aScope ;
    restArg: -1 . 
    "  nameSpace:    not done"
  cst := RubyCompilerState current .
  ( file := cst fileStack topOrNil) ifNotNil:[
     source := file source .  fileName := file fullPath
  ].
  parentMth := cst parentMethodDefOrNil.
  parentMth ifNotNil:[ parentMth setHasBlockArgRef] . "Trac 808"
  (bdy := bodyNode) ifNotNil:[      
     cst pushMethodDef: self .
     [
        bdy walkWithScope: newScop
     ] ensure:[
        cst popMethodDef: self .
     ].
  ].

%


set class RubySClassNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:sclass, ', receiverNode _inspect , ', ', bodyNode _inspect , $]

%

