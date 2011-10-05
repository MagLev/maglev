
set class RubyGlobalVarAliasNode class
category: '*maglev-ast'
method:
s_a: symA b: symB
  | res |
  (res := self _basicNew)
     newName: (RubySymbolNode _basicNew name: symA) 
     oldName: (RubySymbolNode _basicNew name: symB) .
  ^ res

%


set class RubyGlobalVarAliasNode
category: '*maglev-runtime'
method:
irReceiverNode
  | snd| 
  (snd := GsComSendNode new)
     rcvr: (GsComLiteralNode newObject: Object) ;
     stSelector: #nameSpaceForGlobalVar .
  ^ snd

%

