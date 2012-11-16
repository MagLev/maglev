*maglev-runtime
irNode
  | snd cref |
  (cref := RubyConstantRef new) globalVarName: name asSymbol .
  (snd := GsComSendNode new)
     rcvr: (GsComLiteralNode newObject: cref) ;
     stSelector: #resolveGlobalVarValue .
  ^ self ir: snd

