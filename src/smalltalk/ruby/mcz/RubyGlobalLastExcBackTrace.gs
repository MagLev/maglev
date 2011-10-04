
doit
RubyAbstractCallNode subclass: 'RubyGlobalLastExcBackTrace'
	instVarNames: #( rcvr)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalLastExcBackTrace
removeallmethods
removeallclassmethods

set class RubyGlobalLastExcBackTrace class
category: 'as yet unclassified'
method:
newForRp

  | node rcvr |
  node := self _basicNew .
  rcvr := (RubyGlobalLastException _basicNew ) name: #'$!' .
  node rcvr: rcvr .
  ^ node

%


set class RubyGlobalLastExcBackTrace
category: 'as yet unclassified'
method:
name: aSymbol
  "do nothing, name of $@ global not needed by this node"
  ^ self

%


set class RubyGlobalLastExcBackTrace
category: 'as yet unclassified'
method:
rcvr: aNode 
  rcvr := aNode

%


set class RubyGlobalLastExcBackTrace
category: 'as yet unclassified'
method:
receiverNode
  ^ rcvr

%


set class RubyGlobalLastExcBackTrace
category: 'as yet unclassified'
method:
selector
  ^ #backtrace

%

