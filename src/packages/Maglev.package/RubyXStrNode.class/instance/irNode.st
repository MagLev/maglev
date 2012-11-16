*maglev-runtime
irNode 
      "ruby_selector_suffix dependent"
  | str node |
  str := super irNode .
  (node := GsComSendNode new)
    rcvr: ( GsComLiteralNode newObject: Kernel  ) ;
    appendArgument: str ;
    rubySelector: #'__xstr_exec#1__'   .
  self ir: node .
  ^ node

