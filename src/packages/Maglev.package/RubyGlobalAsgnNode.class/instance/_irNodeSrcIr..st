*maglev-runtime
_irNodeSrcIr: srcIr
  | snd cref |
  (cref := RubyConstantRef new) globalVarName: name asSymbol .
  (snd := GsComSendNode new)
    rcvr: (GsComLiteralNode newObject: cref ) ;
    stSelector: #resolveGlobalVarAsgn:  ;
    appendArgument: ( self irValidateArg: srcIr ) .
  self ir: snd .
  ^ snd

