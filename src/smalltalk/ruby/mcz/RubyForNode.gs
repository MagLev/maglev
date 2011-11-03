
set class RubyForNode
category: '*maglev-runtime'
classmethod:
s_a: iter b: var c: body d: srcOfs e: endOfs
  | res |
  (res := self _basicNew )
    init_a: iter b: var c: body d: srcOfs e: endOfs  .
  ^ res

%


set class RubyForNode
category: '*maglev-runtime'
method:
init_a: iter b: var c: body d: srcOfs e: endOfs
  iterNode := iter .
  varNode := var .
  bodyNode := body . endSrcOfs := endOfs .
  self position: srcOfs .

%


set class RubyForNode
category: '*maglev-runtime'
method:
irNode
  | node loopStk  |
  [  | itrIr cmState inline |
   labelRedo := nil . "to allow repeating AST to IR transform"
   itrIr := iterNode irForNode.
   self ir: itrIr .  "install position of 'for' token"
   loopStk := (cmState := RubyCompilerState current)  loopStack .
   labelBreak := (GsComLabelNode new lexLevel: cmState lexicalLevel argForValue: true)  .
   (node := GsComLoopNode new)
       send: itrIr ;    breakLabel:  labelBreak .
   (inline := bodyInline) ifTrue:[ node iterResult: iterNode irIterResult ].
   loopStk push: self  .
   self newBlock:
      [:block | | localLeafs  lexLev asgnNod  |
       self ir: block .
       lexLev := cmState lexicalLevel .
       inline ifTrue:[ 
         varNode buildIrLeafsInto: ( localLeafs := { } ).
         1 to: localLeafs size do:[ :n | | aLocLeaf argLeaf |
           aLocLeaf := localLeafs at: n .
           (argLeaf := GsComVarLeaf new)
                 blockArg: aLocLeaf varName argNumber: n      forBlock: block.
           block appendArg: argLeaf.
           asgnNod :=
              (GsComAssignmentNode _basicNew
                 dest: aLocLeaf
                 source: ( GsComVariableNode new leaf: argLeaf ) ).
           self ir: asgnNod .
           block appendStatement: asgnNod
         ]
       ] ifFalse:[
         varNode buildBlockArgumentsOn: block . 
       ].
       self labelRedo:( GsComLabelNode new lexLevel: lexLev ) .
       labelNext := ( GsComLabelNode new lexLevel: lexLev ).
       bodyNode ifNotNil:[
           block appendStatement: labelRedo  .
           bodyNode irNodeListInto: block .
           block appendStatement: labelNext ;
		 lastSourceOffset: endSrcOfs .
       ].
       itrIr appendArgument: block.
      ]
      isInline: inline .
   ] ensure:[
      loopStk pop: self
   ].
   self ir: node .
   ^ node

%


set class RubyForNode
category: 'accessing'
method:
iterNode

	 ^ iterNode

%


set class RubyForNode
category: 'accessing'
method:
iterNode: aNode
	iterNode := aNode

%


set class RubyForNode
category: 'as yet unclassified'
method:
labelBreak
  ^ labelBreak

%


set class RubyForNode
category: 'as yet unclassified'
method:
setHasRetry
   "sent during walkWithScope " 
   iterNode setForLoopNotInline .
   bodyInline := false

%


set class RubyForNode
category: 'as yet unclassified'
method:
setIsBlockArg: aNode
   ^ self  "do nothing, arg to for loop is a temp "

%


set class RubyForNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
   | itr inline |
    (itr := iterNode) walkIterWithScope: aScope .
    varNode walkWithScope: aScope .
    inline := itr irForNodeWillBeInline. 
    bodyInline := inline .
    inline ifFalse:[
      varNode setIsBlockArg
    ].
    bodyNode walkWithScope: aScope

%


set class RubyForNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:for, ', iterNode _inspect, ', ', varNode _inspect, ', ', bodyNode _inspect , $]


%

