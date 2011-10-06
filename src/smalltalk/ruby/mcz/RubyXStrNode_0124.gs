
set class RubyXStrNode
category: '*maglev-runtime'
method:
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

%


set class RubyXStrNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:xstr, ', value _inspect , $]

%

