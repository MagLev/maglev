*maglev-runtime
buildArgumentsOn: irNode 
    | nodes rest restArray endIdx hasBlk numIrArgs idx num_args  |
    nodes := irArgNodes .
    numIrArgs := nodes size .
    endIdx := numIrArgs .
    (hasBlk := lastArgIsBlk )  ifTrue: [ endIdx := endIdx - 1 ]. 
    idx := 1 .
    num_args := fixedArgCount . "excludes blk arg"
    [ idx <= num_args ] whileTrue:[
        irNode appendArgument: (nodes at: idx) .
        idx := idx + 1
    ].
    idx <= endIdx ifTrue:[ "the args for  * in excess of 3 colon plus *  args  "
      restArray := GsComArrayBuilderNode new.
      [ idx < endIdx] whileTrue:[
         restArray appendElement: (nodes at: idx) .
         idx := idx + 1
      ].
     (self argIsSplatAt: endIdx)  ifTrue:[ | asend | "Fix Trac686"
        (asend := GsComSendNode new) 
           rcvr: restArray ;
           stSelector: #_rubyAddAll:  ;
           appendArgument: (nodes at: endIdx) .
        irNode appendArgument: asend .
      ] ifFalse:[
        restArray appendElement: (nodes at: endIdx).
        irNode appendArgument: restArray .
      ].
    ].
    hasBlk ifTrue:[  
       irNode appendArgument: (nodes at: numIrArgs)
    ].

