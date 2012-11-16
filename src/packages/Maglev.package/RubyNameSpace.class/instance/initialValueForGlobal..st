*maglev-runtime
initialValueForGlobal: aSym
  | res |
  aSym == #'$/'  ifTrue:[  
    "Note, #'$-0'  is translated to #'$/' by   RubyNameSpace>>synonimFor: "
    res := String new:1 .  res at: 1 put: (Character lf) .
    res immediateInvariant .
    ^ res
  ].
  (aSym == #'$:' or: [ aSym ==#'$"' ]) ifTrue: [  ^ { } ] .
  (aSym == #'$VERBOSE' or: [ aSym == #'$DEBUG' ]) ifTrue: [  ^ false ] .
  TraceGlobals >= 2 ifTrue:[
    GsFile gciLogServer:' global ' , aSym printString ,  ' has initial value nil'
  ].
  ^ nil

