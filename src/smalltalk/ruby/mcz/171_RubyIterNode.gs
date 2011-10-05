
doit
RubyNode subclass: 'RubyIterNode'
	instVarNames: #( blockBody bodyNode scope
	                  varNode multArgsNode labelRedo labelNext
	                  zeroDeclaredArgs endSrcOfs)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyIterNode
removeallmethods
removeallclassmethods

set class RubyIterNode
category: '*maglev-ast'
method:
astAnalyzeArgs
  | args |
  args := varNode . 
  args ifNotNil:[
    args == 0 ifTrue:[  
      "zeroDeclaredArgs" 
      "not sure yet where RubyParser produces this case"
       varNode := nil .
       zeroDeclaredArgs := true . 
    ] ifFalse:[ 
      args isSingleIterArg ifTrue:[ 
        " a single DAsgnNode, ok as is"
      ] ifFalse:[ 
        args hasInnerParAssign ifTrue:[
          multArgsNode := args .
          (args := RubyParAsgnStarNode _basicNew) 
          left: { (RubyDAsgnNode _basicNew) name:  #':blkArg1' . 
                  (RubyDAsgnNode _basicNew) name: #':blkArgStar2' }  
        ].
      ].
      varNode := args .  
      self setIsBlockArg: args .
    ]
  ]

%


set class RubyIterNode
category: 'accessing'
method:
blockBody

	 ^ blockBody

%


set class RubyIterNode
category: 'accessing'
method:
blockBody: aInterpretedBlock
   "NOT USED with Sexp parser"
	blockBody := aInterpretedBlock

%


set class RubyIterNode
category: 'accessing'
method:
bodyNode

	 ^ bodyNode

%


set class RubyIterNode
category: '*maglev-runtime'
method:
bodyNode: aBody endOfs: endOfs
  bodyNode := aBody  .
  endSrcOfs := endOfs .

%


set class RubyIterNode
category: 'as yet unclassified'
method:
buildArgumentsOn: irBlock
   | argNod maNod |
   (argNod := varNode) ifNil:[ 
	  zeroDeclaredArgs ifNil:[ irBlock setNoArgsDecl  "for  Proc.new { }.arity == -1" ]	
   ] ifNotNil:[ 
	  argNod buildBlockArgumentsOn: irBlock .  "a DAsgn or ParAsgn"
   ].

%


set class RubyIterNode
category: '*maglev-runtime'
method:
buildComplexArgsOn: irBlock
  | maNod |
  (maNod := multArgsNode )   ifNotNil:[ | irArgs bld send |
         "argNod is a ParAssignSTar  ; maNode has inner ParAsgn "
     (irArgs:= irBlock args) size ~~ 2 ifTrue:[ self error: 'wrong size for iter block args'].
      "RHS of the complex args masgn will be    { blkArg1 } _rubyAddAll: blkArgStar2    "
     (bld := GsComArrayBuilderNode new)
       appendElement: (GsComVariableNode new leaf: (irArgs at: 1)) .
     (send := GsComSendNode new)
        rcvr: bld ;
        stSelector:  #_rubyAddAll:  ;
        appendArgument: (GsComVariableNode new leaf: (irArgs at:2))   .
     irBlock appendStatement:(
          maNod irAssignmentNode: (self ir: send)  ).
  ]

%


set class RubyIterNode
category: '*maglev-runtime'
method:
irNode
    | block loopStk cmState |
    labelRedo := nil . "to allow repeating AST to IR transform"
    [ loopStk := (cmState:= RubyCompilerState current) loopStack .
      block := scope irNewBlockNode.
      self ir: block .
      loopStk push: self .
      self useScope: scope during:[
        self buildArgumentsOn: block .
        scope buildTempsOn: block.
        self buildComplexArgsOn: block .
        bodyNode ifNotNil: [ | lexLev |
          lexLev := cmState lexicalLevel .
          self labelRedo: ( GsComLabelNode new lexLevel: lexLev ).
          labelNext := (GsComLabelNode new lexLevel: lexLev argForValue: true).
          block appendStatement: labelRedo .
          bodyNode irNodeListInto: block .
          block appendStatement: labelNext ; 
               lastSourceOffset: endSrcOfs .
        ]
      ].
     ] ensure:[
       loopStk pop: self
     ]. 
    ^ block

%


set class RubyIterNode
category: 'accessing'
method:
labelBreak
  "sender of irGotoTarget will generate a non-inline IR , 
   to signal a RubyBreakException"
  ^ nil

%


set class RubyIterNode
category: 'accessing'
method:
labelNext
  ^ labelNext

%


set class RubyIterNode
category: 'accessing'
method:
labelRedo
  ^ labelRedo

%


set class RubyIterNode
category: 'accessing'
method:
labelRedo: anObj
  labelRedo ~~ nil ifTrue:[ self error:'redefinition of labelRedo' ].
   labelRedo  := anObj

%


set class RubyIterNode
category: 'as yet unclassified'
method:
multArgsNode: aNode
   multArgsNode := aNode

%


set class RubyIterNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		nextPutAll: ' do |';
		printNode: varNode;
		nextPutAll: '|';
		indentAndEnd: bodyNode

%


set class RubyIterNode
category: 'accessing'
method:
scope

	 ^ scope

%


set class RubyIterNode
category: '*maglev-runtime'
method:
scope: aScope
    scope := aScope

%


set class RubyIterNode
category: 'as yet unclassified'
method:
setIsBlockArg: aNode
   aNode setIsBlockArg

%


set class RubyIterNode
category: 'accessing'
method:
varNode

	 ^ varNode

%


set class RubyIterNode
category: 'accessing'
method:
varNode: aNode 
   aNode == 0 ifTrue:[
	 varNode := nil .
	 zeroDeclaredArgs := true .
   ] ifFalse:[
	 varNode := aNode .
	 self setIsBlockArg: aNode
   ].

%


set class RubyIterNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  | newScop n bdy args mArgs  | 
  scope := (newScop := RubyBlockStaticScope new )
    enclosingScope: aScope ;
    requiredArgs: 0;
    restArg: -1 ;
    _nameSpace: aScope nameSpace  . 
  [ (args := varNode) ifNotNil: [
       varNode walkWithScope: newScop.
       (mArgs := multArgsNode) ifNotNil:[ mArgs walkWithScope: newScop ].
    ].
    (bdy := bodyNode) ifNotNil: [
        bdy walkWithScope: newScop.
     ].
  ] ensure: [
    newScop _nameSpace: nil "avoid commit of tns"
  ]

%


set class RubyIterNode
category: '*maglev-ast'
method:
_varNode: aNode
  varNode := aNode

%

