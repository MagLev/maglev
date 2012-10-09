*maglev-runtime
buildArgumentsOn: irNode argsDescr: optArgsDescr meth: irMethNode 
   "returns boolean tooMany args"
    | inArgsN nFrom moreOutArgs excessInArgs nNeeded argIdx k tooMany firstOpt restUsed |
    nFrom := fromNargs .      tooMany := false . 
    inArgsN := nFrom min: (nNeeded := neededArgs ) .  
    1 to: inArgsN do: [:i |  irNode appendArgument: (self argNode: i).  "args from caller" ].
    moreOutArgs := (nNeeded - inArgsN) max: 0.       argIdx := inArgsN .
    optArgsDescr ~~ 0 ifTrue:[   "callee has default initializers on some args"
      (firstOpt := firstOptArg) ifNil:[             k := 1 .  
         [ k <= moreOutArgs] whileTrue:[ | isOpt |
           isOpt := (optArgsDescr bitAt: argIdx + k) == 1 .
           (isOpt and:[ firstOpt == nil ]) ifTrue:[ 
               firstOpt := argIdx + k.       k := moreOutArgs + 1 "exit while loop" ].
           k := k + 1 .
      ] ].
      firstOpt ifNil:[ firstOpt := SmallInteger maximumValue  ].
      1 to: moreOutArgs do:  [:k | | aargNode isOptArg  |  "needed by callee, not passed by caller"
        argIdx := argIdx + 1 .      restUsed := true .
        isOptArg := (optArgsDescr bitAt: argIdx ) == 1 .
        aargNode := self restArgNode: k forOptArg: isOptArg beforeFirstOpt: (argIdx < firstOpt) .
        irNode appendArgument: aargNode .  ].
    ] ifFalse:[  "no default initializers on callee's args"
        1 to: moreOutArgs do:  [:k | | aargNode   |   "needed by callee, not passed by caller"
          argIdx := argIdx + 1 .       restUsed := true .
          aargNode := self restArgNode: k forOptArg: false beforeFirstOpt: true .
          irNode appendArgument: aargNode .
    ].  ].
    excessInArgs := ( nFrom - inArgsN) max: 0.
    needsRestArg ifTrue:[   "callee wants  a *arg  as last arg"
       irNode appendArgument: 
         (self buildRestAfter: inArgsN extraIn: excessInArgs extraOut: moreOutArgs). 
    ] ifFalse:[  
       excessInArgs ~~ 0 ifTrue:[  tooMany := true ]
         ifFalse:[ restUsed ifNil:[ fromRestArg ifTrue:[ self buildCheckRestEmpty: irMethNode]]].
    ].
    needsBlockArg ifTrue:[  "callee wants a &blk  as last arg"    
       fromBlockArg ifTrue: [irNode appendArgument: self blockNode ]
                  ifFalse: [irNode appendArgument: GsComLiteralNode newNil ]
    ] ifFalse:[
       "NOT USED:  optional block arg not allowed to primitive_nobridge"
       "((primKnd bitAnd:16r2) ~~ 0 and:[fromBlockArg]) ifTrue:[ tooMany := true ].  "
    ].
     ^ tooMany

