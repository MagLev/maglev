*maglev-runtime
buildBodyOn: irNode argsDescr: optArgsDescr 
   "returns boolean isErr  if the bridge method will raise an ArgumentError"
    | send err tooMany tooFew isErr |
    send := GsComSendNode new .
    tooMany := self buildArgumentsOn: send argsDescr: optArgsDescr meth: irNode .
    tooFew := missingArgsError.
    (tooMany or:[ tooFew == true]) ifTrue:[  
      send := self irArgumentErrorNode: tooMany == true .
      isErr := true .
    ] ifFalse:[ 
       send rcvr:  (GsComVariableNode new leaf: (GsComVarLeaf new initializeSelf));
             rubySelectorInBridge: to .
       isErr := false .
       irNode nonBridgeSelector: to .
    ].
    irNode appendStatement: send returnNode .
    ^ isErr 

