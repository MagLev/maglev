*maglev-runtime
installBridgeMethodsFor: argSymbol  in: aClass argsDescr: argsDescrInt optArgs: optArgsDescr
                    protection: protInt primKind: kindArg env: envId
       "During install of primitives,  
       a bridge method will not override any existing ruby method .
       This is to allow coding variants of Ruby methods by hand 
       for things like Object >> send .   
       optArgsDescr is a SmallInteger containing 1 bits for args with default value.
       optArgsDescr is zero if installing bridge methods to a primitive.
       Returns ruby selector prefix of argSymbol  "   
  | inPrims primKnd |
  inPrims :=  installingPrims .
  inPrims ifTrue:[ 
    (self bridgeForPrimAllowed: argSymbol)  ifFalse:[   ^ self "do nothing" ] .
    primKnd :=  kindArg + 4 .
  ] ifFalse:[
     primKnd := kindArg . 
  ].
  ^ RubyBridge installBridgesFor: argSymbol in: aClass argsDescr: argsDescrInt 
           optArgs: optArgsDescr protection: protInt  primKind: primKnd 
           env: envId

