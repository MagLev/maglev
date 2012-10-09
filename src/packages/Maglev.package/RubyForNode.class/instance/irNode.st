*maglev-runtime
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

