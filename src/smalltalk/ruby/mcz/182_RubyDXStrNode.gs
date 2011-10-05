
doit
RubyDStrNode subclass: 'RubyDXStrNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDXStrNode
removeallmethods
removeallclassmethods

set class RubyDXStrNode
category: '*maglev-runtime'
method:
irNode
      "ruby_selector_suffix dependent"
  | dstr node |
  dstr := super irNode .
  (node := GsComSendNode new)
    rcvr: (GsComLiteralNode newObject: Kernel  ) ;
    appendArgument: dstr ;
    rubySelector: #'__xstr_exec#1__'   .
  self ir: node .
  ^ node

%


set class RubyDXStrNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:dxstr, ', self _inspect_list , $]

%

