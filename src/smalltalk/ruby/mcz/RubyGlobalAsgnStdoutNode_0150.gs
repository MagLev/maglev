
set class RubyGlobalAsgnStdoutNode
category: '*maglev-runtime'
method:
irValidateArg: aNode
      "ruby_selector_suffix dependent"
   | send |
   (send := GsComSendNode new)  
     rcvr: (GsComLiteralNode newObject:  GsFile ) ;
     rubySelector:  #'__validate_stdout_value#1__'   ;
     appendArgument:  aNode .
   ^ self ir: send 

%

