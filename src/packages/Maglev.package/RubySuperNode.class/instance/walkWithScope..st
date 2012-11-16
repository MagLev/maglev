(as yet unclassified)
walkWithScope: aScope
  | itr |
  (itr := iterNode) ifNil:[ | args |
	 (args := argsNode) ifNotNil:[
      itr := args getClearIter .
      iterNode := itr .
    ].
  ].
  itr ifNil:[
	 aScope inBootstrap ifTrue:[ implicitBlockArg := false ]
                   ifFalse:[   implicitBlockArg := true .
                            RubyCompilerState current topMethodDef setHasBlockArgRef ]. 
  ] ifNotNil:[
    implicitBlockArg := false .
    itr walkWithScope: aScope . "excluded from argNodes result, so walk here"
  ].
  super walkWithScope: aScope 

