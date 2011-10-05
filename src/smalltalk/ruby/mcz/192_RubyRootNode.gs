
doit
RubyNode subclass: 'RubyRootNode'
	instVarNames: #( bodyNode staticScope sendsBinding
	                  lineNumberBias isMainProgram fileName source)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyRootNode
removeallmethods
removeallclassmethods

set class RubyRootNode
category: 'accessing'
method:
bodyNode

	 ^ bodyNode

%


set class RubyRootNode
category: 'accessing'
method:
bodyNode: aNode
	bodyNode := aNode .
	

%


set class RubyRootNode
category: '*maglev-runtime'
method:
buildTopBindingOn: irMeth
      "ruby_selector_suffix dependent"
  | bnd put  |
  (bnd := GsComSendNode new)
     rcvr:  GsComVariableNode newSelf ;
     rubySelector:  #'binding#1__'  ;
     appendArgument: (GsComLiteralNode newObject: (RubyLexicalPath withAll: { Object }) ) .
  (put := GsComSendNode new) 
    rcvr: (GsComLiteralNode newObject: RubyContext ) ;
    stSelector:  #installTopBinding:  ;
    appendArgument:  (self ir: bnd) .

  irMeth appendStatement: ( self ir: put  )  .

%


set class RubyRootNode
category: 'as yet unclassified'
method:
comIrMethNode
   ^ nil  "not a method definition"

%


set class RubyRootNode
category: 'parsetree-as yet unclassified'
method:
detectMismatchWith: other
	(bodyNode detectMismatchWith: other bodyNode) ifNotNilDo: [:n | ^ n].
	^ staticScope detectMismatchWith: other staticScope

%


set class RubyRootNode
category: 'as yet unclassified'
method:
fileName
  ^ fileName

%


set class RubyRootNode
category: 'as yet unclassified'
method:
fileName: nameString source: srcString
  fileName := nameString .
  source := srcString

%


set class RubyRootNode
category: 'as yet unclassified'
method:
hasSource
  ^ source ~~ nil

%


set class RubyRootNode
category: '*maglev-runtime'
method:
irMethodNode: envId forClass: aClass
    | node cst num bias  |
    cst := RubyCompilerState current .
    envId == cst envId ifFalse:[ self error:'RubyRootNode>>irMethodNode: inconsistent envId'].
    (node := GsComMethNode newRuby) environment: envId ;
       setRubyLineNumberBias: (bias := lineNumberBias) ;
       lineNumber: 1 "to go with byte offsets from RubyParser" ;
       fileName: fileName source: source .
    cst pushMethodDef: self lineBias: bias .
    [
      staticScope buildTempsOn: node.
      self useScope: staticScope during: [
        isMainProgram == true ifTrue:[ self buildTopBindingOn: node ].
        bodyNode buildStatementsOn: node
      ].
      sendsBinding == true ifTrue:[ node forceAllArgsTmpsToVc  ].
      self ir: node.
    ] ensure:[
      cst popMethodDef: self lineBias: bias
    ].
    ^ node

%


set class RubyRootNode
category: 'converting'
method:
irNode
   isMainProgram == true ifTrue:[ self error:'main program should not be here'].
	^ bodyNode irNode

%


set class RubyRootNode
category: '*maglev-runtime'
method:
irNodeListInto: blockIr
    ^ bodyNode irNodeListInto: blockIr

%


set class RubyRootNode
category: 'as yet unclassified'
method:
lineBias
  ^ lineNumberBias

%


set class RubyRootNode
category: 'as yet unclassified'
method:
lineBias: anInt
  lineNumberBias := anInt

%


set class RubyRootNode
category: '*maglev-debugging'
method:
lineForOffset: aByteOffset
  "returns a one-based line number corresponding to the
   1-based byte offset."

  ^ source lineForOffset: aByteOffset

%


set class RubyRootNode
category: 'parsetree-test'
method:
matches: other
	^ self species = other species and: [bodyNode matches: other bodyNode]

%


set class RubyRootNode
category: 'as yet unclassified'
method:
methodSelector
  ^ nil

%


set class RubyRootNode
category: 'printing'
method:
printSourceOn: aStream
	bodyNode printSourceOn: aStream

%


set class RubyRootNode
category: 'as yet unclassified'
method:
scope
  ^ staticScope

%


set class RubyRootNode
category: '*maglev-runtime'
method:
selectorForSuper
   ^ #__super_called_outside_of_method

%


set class RubyRootNode
category: 'as yet unclassified'
method:
selectorPrefix
  ^ ''  "selector of a rootNode is #'' "

%


set class RubyRootNode
category: 'as yet unclassified'
method:
setBodyHasBlockGiven
   "do nothing, no implicit block leaf in a main program"
   ^ self

%


set class RubyRootNode
category: 'as yet unclassified'
method:
setHasBlockArgRef
  ^ true  "sender must handle yield in an eval"

%


set class RubyRootNode
category: 'as yet unclassified'
method:
setMainProgram
  sendsBinding := true .
  isMainProgram ifNil:[ isMainProgram := true ].

%


set class RubyRootNode
category: 'as yet unclassified'
method:
setSendsBinding 
   sendsBinding := true

%


set class RubyRootNode
category: 'as yet unclassified'
method:
source
  ^ source

%


set class RubyRootNode
category: 'as yet unclassified'
method:
source: srcString
  source := srcString

%


set class RubyRootNode
category: 'accessing'
method:
staticScope

	 ^ staticScope

%


set class RubyRootNode
category: '*maglev-runtime'
method:
staticScope: aScope
    staticScope := aScope

%


set class RubyRootNode
category: '*maglev-runtime'
method:
walkInEval
  ^ true

%


set class RubyRootNode
category: '*maglev-runtime'
method:
walkScopes: topScopN
  |  inBoot cst newScop |
  cst := RubyCompilerState current .
  inBoot := cst installingPrims .
  inBoot ifTrue:[  isMainProgram := false ].
  staticScope := (newScop := RubyLocalStaticScope new).
  newScop
    requiredArgs: 0;
    restArg: -1 .
  (topScopN isKindOf: RubyStaticScope) ifTrue:[  "fix 829"
    newScop enclosingScope:  topScopN 
  ] ifFalse:[
    newScop _nameSpace:  topScopN"a Module" 
  ].
  newScop
    inBootstrap:  inBoot  ;
    clearVariableNames .
  [ bodyNode ifNotNil:[
      cst pushMethodDef: self .
      [
        bodyNode walkWithScope: staticScope
      ] ensure:[
        cst popMethodDef: self
      ].
    ].
  ] ensure:[
    newScop _nameSpace: nil "avoid commit of tns"
  ]

%


set class RubyRootNode
category: '*maglev-runtime'
method:
_inspect
  ^  bodyNode _inspect

%

