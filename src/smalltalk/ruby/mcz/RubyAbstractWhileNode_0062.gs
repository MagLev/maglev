
set class RubyAbstractWhileNode class
category: '*maglev-runtime'
method:
s_forRp: condArg block: blkArg bool: cfBool
  | cls  node |
  cls := cfBool ifTrue:[  RubyWhileNode ] ifFalse:[ RubyUntilNode ].
  (node := cls _basicNew )
     conditionNode: condArg ;
     bodyNode: blkArg ;
     conditionFirst: cfBool ;
     baseSelector: (self selectorFor: cfBool ) .
  ^ node

%


set class RubyAbstractWhileNode
category: 'as yet unclassified'
method:
baseSelector: sym
  baseSel := sym

%


set class RubyAbstractWhileNode
category: 'accessing'
method:
bodyNode

	 ^ bodyNode

%


set class RubyAbstractWhileNode
category: 'accessing'
method:
bodyNode: aNode
	bodyNode := aNode

%


set class RubyAbstractWhileNode
category: 'as yet unclassified'
method:
breakHandler
  ^ [ :ex | |args | 
	     ( (args := ex gsArguments) at: 1) == 0 ifTrue:[ 
		          ex return: (args at: 2) "terminate the while loop"
		   ] ifFalse:[ ex pass ]
	 ]

%


set class RubyAbstractWhileNode
category: 'as yet unclassified'
method:
conditionFirst: aBool

  condIsFirst :=  aBool

%


set class RubyAbstractWhileNode
category: 'accessing'
method:
conditionNode
  "a ruby primitive, in env 2"
   ^ conditionNode

%


set class RubyAbstractWhileNode
category: 'accessing'
method:
conditionNode: aNode
	conditionNode := aNode

%


set class RubyAbstractWhileNode
category: '*maglev-runtime'
method:
irBodyNode
  ^ self newBlock: [:block |         
        block appendStatement:  labelRedo .                        
        bodyNode irNodeListInto: block .
        block appendStatement: labelNext .
       block 
     ] isInline: blksInline .

%


set class RubyAbstractWhileNode
category: 'as yet unclassified'
method:
irConditionNode
  ^ self newBlock:[:block |
		block appendStatement: conditionNode irEvaluatedRcvrNode.
		block 
		]
	   isInline: blksInline

%


set class RubyAbstractWhileNode
category: 'accessing'
method:
irNode
  |  snd  loopStk res cmState | 
  loopStk := (cmState := RubyCompilerState current)  loopStack .
  loopStk push: self .
  [ | lexLev |
	 lexLev := cmState lexicalLevel .
	 labelBreak := (GsComLabelNode new lexLevel: lexLev argForValue: true)  .  
	 labelNext := ( GsComLabelNode new  lexLevel: lexLev)  . 
	 labelRedo := ( GsComLabelNode new  lexLevel: lexLev)  . 
	 snd := super irNode  .
	 blksInline ifTrue:[ 
		(res := GsComLoopNode new)  send: snd ;  breakLabel:  labelBreak .
	 ] ifFalse:[ 
		res := snd 
     ].
  ] ensure:[
    loopStk pop: self 
  ].
  ^  res 
	

%


set class RubyAbstractWhileNode
category: 'converting'
method:
isSmalltalkSend
	^ true  "either a send like _ruby_whileTrue1:  
	           or a whileTrue: in ruby method that gets ruby branch bytecode"

%


set class RubyAbstractWhileNode
category: 'accessing'
method:
labelBreak
  ^ labelBreak

%


set class RubyAbstractWhileNode
category: 'accessing'
method:
labelNext
  ^ labelNext

%


set class RubyAbstractWhileNode
category: 'accessing'
method:
labelRedo
  ^ labelRedo

%


set class RubyAbstractWhileNode
category: 'as yet unclassified'
method:
nextHandler
  ^ [ :ex |  (ex gsArguments at: 1) == 1 ifTrue:[ 
		          ex retry " restart the on:do: for which this block is handler"
		      ] ifFalse:[ ex pass ]
	 ]

%


set class RubyAbstractWhileNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		nextPutAll: self nameForPrint ;
		parenthesize: conditionNode;
		indentAndEnd: bodyNode

%


set class RubyAbstractWhileNode
category: '*maglev-runtime'
method:
selector
      "ruby_selector_suffix dependent"
  | base sel |
  base := baseSel .
  blksInline ifTrue:[  " a ruby send"
     ^ base _asSymbolWithRubySuffix: 16r4 "#1__" 
  ].
  "a smalltalk send such as  #'_ruby_whileTrue: "
  sel := '_ruby_' , base .
  sel add: $: .
  ^ sel asSymbol  

%


set class RubyAbstractWhileNode
category: 'as yet unclassified'
method:
setHasBeginRescue
  hasBeginOrRescue := true

%


set class RubyAbstractWhileNode
category: '*maglev-runtime'
method:
setHasBreakNext
  hasBreakOrNext := true  "hasBreakOrNext currently not used"

%


set class RubyAbstractWhileNode
category: 'as yet unclassified'
method:
setHasRetry
  ^ self " do nothing"

%


set class RubyAbstractWhileNode
category: 'converting'
method:
shouldOptimize
	^ blksInline 

%


set class RubyAbstractWhileNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  " self receiverNode == nil ;  self argNodes == #()"
  | loopStk |
  loopStk := RubyCompilerState current loopStack .
  loopStk push: self .
  [ 
    conditionNode walkWithScope: aScope . 
    bodyNode walkWithScope: aScope .
  ] ensure:[
     loopStk pop: self
  ].
  blksInline :=  hasBeginOrRescue == nil

%

