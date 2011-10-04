
doit
RubyAbstractCallNode subclass: 'RubyUndefNode'
	instVarNames: #( name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyUndefNode
removeallmethods
removeallclassmethods

set class RubyUndefNode
category: 'accessing'
method:
argNodes
	^ #( )

%


set class RubyUndefNode
category: 'as yet unclassified'
method:
irArgNodes
  ^ { GsComLiteralNode newObject: name "a Symbol" }

%


set class RubyUndefNode
category: '*maglev-runtime'
method:
irReceiverNode 
  | node |
    (node := GsComSendNode new) 
       rcvr: ( GsComLiteralNode newObject: GsProcess ) ;
       stSelector:  #methodDefnTarget .
    self ir: node .
    ^ node 

%


set class RubyUndefNode
category: 'as yet unclassified'
method:
isSameAs: other
	^ self name = other name

%


set class RubyUndefNode
category: 'accessing'
method:
isSmalltalkSend
	^ true

%


set class RubyUndefNode
category: 'accessing'
method:
name

	 ^ name

%


set class RubyUndefNode
category: '*maglev-runtime'
method:
name: aName
    name := aName

%


set class RubyUndefNode
category: 'accessing'
method:
receiverNode
	^ RubySelfNode _basicNew

%


set class RubyUndefNode
category: '*maglev-runtime'
method:
selector
    ^ #rubyUndefMethod:

%


set class RubyUndefNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:undef ,',  name  , $]

%

