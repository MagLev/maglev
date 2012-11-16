*maglev-runtime
buildExecRest: optArgsDescr 
    | moreOutArgs nNeeded argIdx k firstOpt restUsed aryB |
    aryB := GsComArrayBuilderNode _basicNew initialize . 
    fromNargs ~~ 0 ifTrue:[ self error:'expect 0 incoming colons'].
    fromRestArg ifFalse:[ self error:'expected incoming star' ].
    nNeeded := neededArgs .
    moreOutArgs := nNeeded  . argIdx := 0 .
    optArgsDescr ~~ 0 ifTrue:[   "callee has default initializers on some args"
      (firstOpt := firstOptArg) ifNil:[             k := 1 .  
         [ k <= moreOutArgs] whileTrue:[ | isOpt |
           isOpt := (optArgsDescr bitAt: argIdx + k) == 1 .
           (isOpt and:[ firstOpt == nil ]) ifTrue:[ 
               firstOpt := argIdx + k.       k := moreOutArgs + 1 "exit while loop" ].
           k := k + 1 .
      ] ].
      firstOpt ifNil:[ firstOpt := SmallInteger maximumValue  ].
      1 to: moreOutArgs do:  [:k | | aargNode isOptArg  |   "args needed by callee, not passed by caller"
        argIdx := argIdx + 1 .      restUsed := true .
        isOptArg := (optArgsDescr bitAt: argIdx ) == 1 .
        aargNode := self restArgNode: k forOptArg: isOptArg beforeFirstOpt: (argIdx < firstOpt) .
        aryB appendElement: aargNode .  
      ].
    ] ifFalse:[  "no default initializers on callee's args"
        1 to: moreOutArgs do:  [:k | | aargNode   |   "args needed by callee, not passed by caller"
          argIdx := argIdx + 1 .       restUsed := true .
          aargNode := self restArgNode: k forOptArg: false beforeFirstOpt: true .
          aryB appendElement: aargNode .
        ].  
    ].
    needsRestArg ifTrue:[   "callee wants  a *arg  as last arg"
        aryB appendElement: (self buildRestAfter: 0 extraIn: 0 extraOut: moreOutArgs) .
    ].
    needsBlockArg ifTrue:[  "callee wants a &blk  as last arg"
      "callee wants a &blk  as last arg"    
       fromBlockArg ifTrue: [ aryB appendElement: self blockNode ]
                  ifFalse: [ self error:'expected incoming block' ].
    ].
    needsRestArg ifTrue:[
      ^ aryB  "buildRestAfter:... done above"
    ] ifFalse:[ | send |
      "add remaining args and let too many args be signaled by execMethod primitive"
       (send := GsComSendNode new) rcvr: aryB ;
          stSelector:  #addAll:excludingFirst: ;
          appendArgument: self restNode ;
          appendArgument: (GsComLiteralNode newInteger: moreOutArgs) .
       ^ send
    ].

