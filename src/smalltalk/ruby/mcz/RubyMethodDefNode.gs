
set class RubyMethodDefNode
category: '*maglev-runtime'
classmethod:
s_a: nameSym b: args c: body
  | node |
  ( node := self _basicNew )
     nameNode:  nameSym  ;
     lineBias: 0 ;
     argsNode: args ;
     bodyNode: body .
   ^ node

%


set class RubyMethodDefNode
category: 'accessing'
method:
argsNode

	 ^ argsNode

%


set class RubyMethodDefNode
category: 'accessing'
method:
argsNode: aArgsNode
	argsNode := aArgsNode

%


set class RubyMethodDefNode
category: 'accessing'
method:
bodyNode

	 ^ bodyNode

%


set class RubyMethodDefNode
category: 'accessing'
method:
bodyNode: aNode
	bodyNode := aNode

%


set class RubyMethodDefNode
category: 'converting'
method:
buildOptArgsOn: aNode
   | optargs |
   (optargs := argsNode optArgs)  ifNotNil:
		[ optargs list do:
			[:ea |
			aNode appendStatement: ea irOptArgNode]]

%


set class RubyMethodDefNode
category: 'as yet unclassified'
method:
childrenForMatch
	^ super childrenForMatch, {self argsNode. self bodyNode.  self scope}

%


set class RubyMethodDefNode
category: 'as yet unclassified'
method:
comIrMethNode
  ^ irMethNode

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
defTarget: aScope
  "Return a RubyMethodDefTarget, creating one if necessary"
  ^ defTarget ifNil:[ defTarget := RubyMethodDefTarget _basicNew ]

%


set class RubyMethodDefNode
category: 'as yet unclassified'
method:
fileName
  ^ fileName

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
hasInnerDefs
  ^ innerDefs == 1

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
innerDefs
  ^ innerDefs 

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
irArgNodes
    |  bodyLeaf   | 
      "nameLeaf := GsComLitLeaf new symbolLiteral: ( self methodSelector) .
       name will be obtained during IR generation of the method itself.
      "
    bodyLeaf := GsComLitLeaf new methodLiteral: self.
    ^      {  self irTargetNode .
          GsComLiteralNode new leaf: bodyLeaf  }

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
irMethodNode: envId forClass: aClass 
      "ruby_selector_suffix dependent"

^ irMethNode ifNil:[  "Fix Trac770, only run AST to IR once on a given AST"
    self buildIrMethodNode: [ :node | | mSel prefix  |
      irMethNode := node .
      node fileName: fileName source: source ;
         lineNumber:  startLine ; endSourceOffset: endOffset .
      self ir: node  "get start source offset".
      prefix := nameNode .  "a Symbol, not a RubyArgumentNode"
      scope extraArgs: argsNode extraArgs .
      self useScope: scope during:[ | lastNormalArgIdx |
         lastNormalArgIdx := argsNode buildMethodArgumentsOn: node implicitBlk: hasBlockArgRef .
         (prefix _rubyAt1: -1) ==  61"$= asciiValue"  ifTrue:[ | secLast |
           secLast := prefix _rubyAt1: -2 .
           secLast ifNotNil:[ 
             secLast := Character withValue: secLast .
             (secLast == $]  or:[ secLast isAlphaNumeric]) ifTrue:[
               "Example:  def foo=(a,b); end   # default return value is b , per Trac 785"
               node rubyDefaultReturnArg: lastNormalArgIdx .
             ].
           ].
         ].
         mSel := (prefix , argsNode selectorSuffix) asSymbol .
         methSelector := mSel .
         hasBlockArgRef ifTrue: [
	     "add a temp to keep the implicit block around. If we have an explicit block argument,
            s/o might assign to it, and we loose the original block reference
            (github.com/MagLev/maglev/issues/69)"
	     self optionallyBuildImplicitBlockTemp].
         scope buildTempsOn: node .
         self buildOptArgsOn: node.
         (prefix == #initialize or:[ prefix == #initialize_copy]) ifTrue:[
            aClass isMeta ifFalse:[
               "initialize, initialize_copy  instance methods  private by default."
               node setRubyPrivate .
            ].
         ].  
         defTarget ifNotNil:[ :dt | | rtCst snd |  "this is an outer def"
           (rtCst := GsComSendNode new)
              rcvr: (GsComLiteralNode newObject: RubyCompilerState) ;
              stSelector: #current .
           (snd := GsComSendNode new)
              rcvr: rtCst ;
              stSelector: #outerDefLexPath: ;
              appendArgument: (GsComLiteralNode newObject: 
            RubyCompilerState current rtModuleLexPath) .
           node appendStatement: snd .
       (snd := GsComSendNode new)
         rcvr: (GsComLiteralNode newObject: dt) ;
         stSelector:  self defTargetAssignSelector  ;
         appendArgument: ( GsComLiteralNode newObject: aClass) . 
       node appendStatement: snd .
         ].
         self bodyNode buildStatementsOn: node .
      ].
      sendsBinding == true ifTrue:[ node forceAllArgsTmpsToVc ].
    ]
  ]

%


set class RubyMethodDefNode
category: 'accessing'
method:
irReceiverNode
	^ self ir:( self irCompilerNode)

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
isInnerDef
   "Used by RubyColon2Node only .
    Return true if self is an inner def not counting defs above an eval"

   ^ outerDef ifNotNil:[:o | o ~~ self ] ifNil:[ false ]

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
isMethodDef
  ^ true

%


set class RubyMethodDefNode
category: 'converting'
method:
isMethodDefinition
	^ true

%


set class RubyMethodDefNode
category: 'converting'
method:
isSmalltalkSend
	^ true

%


set class RubyMethodDefNode
category: 'as yet unclassified'
method:
lineBias
  ^ lineBias

%


set class RubyMethodDefNode
category: 'as yet unclassified'
method:
lineBias: anInt
  lineBias := anInt

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
lineForOffset: anOffset
  source ifNotNil:[ ^ source lineForOffset: anOffset ].
  ^ startLine "ignore anOffset"

%


set class RubyMethodDefNode
category: 'converting'
method:
methodSelector
    "Useable only after IR generation of method's declared args"
   ^ methSelector 
	

%


set class RubyMethodDefNode
category: 'accessing'
method:
nameNode: aArgumentNode
	nameNode := aArgumentNode

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
optionallyBuildImplicitBlockTemp
      "github.com/MagLev/maglev/issues/69"
      "If we have an explicit block argument, s/o in ruby might assign to it. We save the original block's reference
       to a temp under the #implicitBlockTempName, so yield+Proc.new can still work"
      "NOTE: Walking nodes in the scope is fine here, these AST nodes are newly created"
	| blockArg |
	(blockArg := argsNode blockArgNode) ifNotNil: [| explicitBlockVar assignment |
		scope addImplicitBlockTemp.
		explicitBlockVar := (RubyImplicitBlockVarNode s_a: blockArg name).
		explicitBlockVar walkWithScope: scope.
		assignment := RubyLocalAsgnNode s_a: scope implicitBlockTempName b: 0 c: explicitBlockVar.
		assignment walkWithScope: scope.
		self bodyNode prepend_to_block: assignment.
	].

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
outerDef
  "return the outermost def"
  ^ outerDef ifNotNil:[:o | o == self ifTrue:[ o ] ifFalse: [outerDef] ] 
              ifNil:[ self ]

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
outerDef: aNode
  outerDef := aNode

%


set class RubyMethodDefNode
category: 'accessing'
method:
scope

	 ^ scope

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
selectorForSuper

  ^ nameNode "a Symbol"

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
selectorPrefix
    ^ nameNode "a Symbol"

%


set class RubyMethodDefNode
category: 'converting'
method:
setHasBlockArgRef
  hasBlockArgRef := true .
  ^ false "not in an eval"

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
setHasInnerDefs
  innerDefs := 1

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
setHasInnerEvalOrDef
  defTarget ifNil:[ defTarget := RubyMethodDefTarget _basicNew ]

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
setIsInnerDef: outer
  innerDefs := 2 .

%


set class RubyMethodDefNode
category: 'as yet unclassified'
method:
setSendsBinding
   sendsBinding := true .
   hasBlockArgRef := true . "implicit block ref so binding captures the block arg"

%


set class RubyMethodDefNode
category: 'as yet unclassified'
method:
setTrace: aBool
  traceBool := aBool

%


set class RubyMethodDefNode
category: 'as yet unclassified'
method:
source
  ^ source

%


set class RubyMethodDefNode
category: 'as yet unclassified'
method:
startLine: anInt
  startLine := anInt

%


set class RubyMethodDefNode
category: '*maglev-debugging'
method:
startLine: anInt endOffset: endOfs
  startLine := anInt  .
  endOffset := endOfs .

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
walkInEval
  ^ false

%


set class RubyMethodDefNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
      "ruby_selector_suffix dependent"
  | newScop cst file |
  scope := (newScop := RubyLocalStaticScope  new). 
  newScop 
    nonInheritingChildOf: aScope ;
    "no special init for extraArgs. all args appear to be walked before the temps" 
    _nameSpace: aScope nameSpace  .
  cst := RubyCompilerState current.
  cst pushMethodDef: self scope: aScope .
  
  ( file := cst fileStack topOrNil) ifNotNil:[ 
       fileName := file fullPath .  source :=  file source 
  ].
  [ | prefix |
    prefix := nameNode .
    ((prefix _rubyAt1: -1) == 108"$l asciiValue"  and:[
          RubyAbstractCallNode evalSelectors includesIdentical: prefix ]) ifTrue:[
      "required to avoid tooManyArgs errors since we synthesize file and line args
       to any call to an eval "
      argsNode ensureStarArg .  "moved to here to Fix Trac 918"
    ].
    argsNode walkWithScope: newScop  .
    bodyNode walkWithScope: newScop .
    hasBlockArgRef ifNil:[ hasBlockArgRef := false ].
  ] ensure:[
    cst popMethodDef: self.
    newScop _nameSpace: nil "avoid commit of tns"
  ].

%

