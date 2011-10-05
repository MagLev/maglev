
set class RubyAliasNode class
category: '*maglev-ast'
method:
s_a: objA b: objB  c: srcOffsetSi
  | res |
  (res := self _basicNew ) 
     newName: objA oldName: objB ;
     position: srcOffsetSi .
  ^ res

%


set class RubyAliasNode
category: 'converting'
method:
argNodes 

	^ { newName . oldName  }

%


set class RubyAliasNode
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


set class RubyAliasNode
category: 'parsetree-test'
method:
isSameAs: other
	^ self newName = other newName
		and: [self oldName = other oldName]

%


set class RubyAliasNode
category: 'converting'
method:
isSmalltalkSend
	^ true

%


set class RubyAliasNode
category: 'accessing'
method:
newName

	 ^ newName

%


set class RubyAliasNode
category: 'accessing'
method:
newName: aString
	newName := aString

%


set class RubyAliasNode
category: '*maglev-ast'
method:
newName: objA oldName: objB
  newName := objA .
  oldName := objB

%


set class RubyAliasNode
category: 'accessing'
method:
oldName

	 ^ oldName

%


set class RubyAliasNode
category: 'accessing'
method:
oldName: aString
	oldName := aString

%


set class RubyAliasNode
category: 'converting'
method:
receiverNode
	^ RubySelfNode _basicNew

%


set class RubyAliasNode
category: 'converting'
method:
selector
	^ #rubyAlias:from:

%


set class RubyAliasNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:alias, ' , oldName _inspect , ', ' , newName _inspect, $]

%

