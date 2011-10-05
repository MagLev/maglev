
doit
RubyGlobalAsgnNode subclass: 'RubyGlobalAsgnStdoutNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalAsgnStdoutNode
removeallmethods
removeallclassmethods

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

