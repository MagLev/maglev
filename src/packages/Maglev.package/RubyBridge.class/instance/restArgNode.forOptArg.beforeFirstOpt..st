*maglev-runtime
restArgNode: argNum forOptArg:  isOptArg beforeFirstOpt: beforeFirstOpt
  "build arg for an arg needed by callee,  not passed by caller.
   isOptArg is true if the argument has a 
   default value assignment in the parameter declaration.
   argNum is one based "
  | stSel stArg |
  fromRestArg ifFalse: [
   ^ isOptArg ifTrue:[  self noArgNode  ]
       ifFalse:[ (needsRestArg or:[ beforeFirstOpt]) ifTrue:[ missingArgsError := true ].
                 GsComLiteralNode newNil ].
  ].
  stArg := argNum .
  isOptArg ifTrue:[ 
     stSel := #atOrNoArg: .  
     ( fromRestArg and:[ needsRestArg ~~ true ])  ifTrue:[ 
           stArg := stArg bitOr: ((neededArgs + 1) bitShift: 8)   "encode maxArgs"
     ].
  ] ifFalse:[ 
     beforeFirstOpt ifTrue:[
       stSel := #atOrMissingArgErr: . 
       ( fromRestArg and:[ needsRestArg ~~ true ])  ifTrue:[ 
           stArg := stArg bitOr: ((neededArgs + 1) bitShift: 8)   "encode maxArgs"
       ].
     ] ifFalse:[ 
       stSel := #atOrNil: 
   ] ].
  ^ GsComSendNode new
      rcvr: self restNode ;  "at runtime rcvr will be an Array"
      stSelector:  stSel  ;
    appendArgument: (GsComLiteralNode newInteger: stArg)

