*maglev-runtime
typeCheckNodeFor: valueIrNode
      "ruby_selector_suffix dependent"
  | node |
  (node := GsComSendNode new)
     rcvr: ( GsComLiteralNode newObject: Exception ); "Object rubyGlobalsAt:#Exception"
      rubySelector:  #'_validate#1__'  ;
      appendArgument: valueIrNode .
  ^ self ir: node 

