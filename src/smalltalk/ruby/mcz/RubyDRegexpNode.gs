
doit
RubyListNode subclass: 'RubyDRegexpNode'
	instVarNames: #( options)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDRegexpNode
removeallmethods
removeallclassmethods

set class RubyDRegexpNode
category: '*maglev-ast'
method:
as_cond
  ^ RubyMatch2Node s_a: self b: ( RubyGlobalVarNode s_a:  #'$_' )

%


set class RubyDRegexpNode
category: 'accessing'
method:
fixedOptions
  ^ options ifNil:[ 0 ]      
  

%


set class RubyDRegexpNode
category: '*maglev-ast'
method:
irNode
      "ruby_selector_suffix dependent"
    | node array lst |
    array := GsComArrayBuilderNode new.
    lst := list .
    1 to: lst size do: [:n | array appendElement: (lst at: n) irNode].
    (node := GsComSendNode new)
            rcvr: array;
            rubySelector: #'__joinStringsWithRegexpOptions#1__'  ;
            appendArgument: (RubyFixnumNode newForInt: self fixedOptions) irNode.
    self ir: array; ir: node.
    ^ node

%


set class RubyDRegexpNode
category: 'accessing'
method:
options

	 ^ options

%


set class RubyDRegexpNode
category: 'accessing'
method:
options: aNumber
	options := aNumber

%


set class RubyDRegexpNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:dregex, ', self _inspect_list , $]

%

