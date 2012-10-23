*maglev-runtime
buildRestAfter: inArgsUsed extraIn: excessInArgs  extraOut: moreOutArgs
  | aryB snd |
  excessInArgs ~~ 0 ifTrue:[ 
    aryB := GsComArrayBuilderNode new.
    1 to:  excessInArgs do: [:m | aryB appendElement: (self argNode: inArgsUsed + m )  ].
    fromRestArg ifTrue:[
     (snd := GsComSendNode new) rcvr: aryB .
      moreOutArgs == 0 ifTrue:[
        snd  stSelector:  #_rubyAddAll: ; appendArgument: self restNode .
      ] ifFalse:[
       snd stSelector: #addAll:excludingFirst: ;
          appendArgument: self restNode ;
          appendArgument: (GsComLiteralNode newInteger: moreOutArgs) .
      ].
      ^ snd
    ] ifFalse:[
      ^ aryB
    ]
  ] ifFalse:[
    moreOutArgs == 0 ifTrue:[
      fromRestArg ifTrue:[ 
        ^ GsComVariableNode new leaf: (self restLeaf) 
      ] ifFalse:[
        ^ GsComArrayBuilderNode new
      ].
    ] ifFalse:[
      fromRestArg ifTrue:[
        aryB := GsComArrayBuilderNode new.
        (snd := GsComSendNode new) rcvr: aryB ;
          stSelector:  #addAll:excludingFirst: ;
          appendArgument: self restNode ;
          appendArgument: (GsComLiteralNode newInteger: moreOutArgs) .
        ^ snd
      ] ifFalse:[
        ^ GsComArrayBuilderNode new 
      ].
    ].
  ]. 

