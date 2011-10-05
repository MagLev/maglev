
set class RubyGlobalLastExcBackTrace class
category: 'as yet unclassified'
method:
newForRp

  | node receiver |
  node := self _basicNew .
  receiver := (RubyGlobalLastException _basicNew ) name: #'$!' .
  node rcvr: receiver .
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

