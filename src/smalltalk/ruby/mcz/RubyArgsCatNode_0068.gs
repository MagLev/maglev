
set class RubyArgsCatNode
category: '*maglev-runtime'
classmethod:
comment
  ^ 'RubyArgsCatNode is only used in AST produced by MRI parse server
     when loading bootstrap code.  
     RubyParser uses instances of RubyRpCallArgs instead.'

%


set class RubyArgsCatNode
category: '*maglev-runtime'
method:
argNodes
   ^ firstNode argNodes copyWith: secondNode 

%


set class RubyArgsCatNode
category: 'as yet unclassified'
method:
fCallArgNodes
  ^ firstNode argNodes copyWith: secondNode

%


set class RubyArgsCatNode
category: 'accessing'
method:
firstNode: aNode
	firstNode := aNode

%


set class RubyArgsCatNode
category: '*maglev-runtime'
method:
getClearIter
  ^ nil 

%


set class RubyArgsCatNode
category: 'converting'
method:
hasRestArg
   ^ false

%


set class RubyArgsCatNode
category: 'as yet unclassified'
method:
isArgsCatNode
  ^ true

%


set class RubyArgsCatNode
category: 'as yet unclassified'
method:
isSmalltalkSend
  ^ false

%


set class RubyArgsCatNode
category: 'converting'
method:
numArgs
	^ 1

%


set class RubyArgsCatNode
category: 'as yet unclassified'
method:
receiverNode
  ^ firstNode

%


set class RubyArgsCatNode
category: 'accessing'
method:
secondNode: aNode
	secondNode := aNode

%


set class RubyArgsCatNode
category: 'converting'
method:
selector
  "Receiver at runtime should be an Array"
   ^ #__add_arguments

%

