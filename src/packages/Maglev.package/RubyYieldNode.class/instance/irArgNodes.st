*maglev-runtime
irArgNodes
      "ruby_selector_suffix dependent"
  | args |  
  (args := argsNode) ifNotNil:[ 
    args class == RubyRpCallArgs ifTrue:[
      args irYieldArgs ifNotNil:[ :yArgs | 
        forceSelector := yArgs at: 1  .
        ^ { yArgs at: 2 }
      ].
    ] ifFalse:[
      args isSplatNode ifTrue:[
        forceSelector := #'call#0*_' .
         ^ { args irYieldStarNode } 
      ].
    ].
  ].
  ^ super irArgNodes 

