*maglev-runtime
irDefinedQNode
  " generated code does not use a RubyConstantRef to cache the ref"
  | snd|
  (snd := GsComSendNode new)
     rcvr: (GsComLiteralNode newObject: Object) ;
     stSelector: #rubyGlobalVarDefinedQ: ;
      appendArgument: (GsComLiteralNode newObject: name asSymbol) .
  ^ self ir: snd

