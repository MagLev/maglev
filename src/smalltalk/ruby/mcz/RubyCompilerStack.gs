
set class RubyCompilerStack
category: '(as yet unclassified)'
method:
depth
  ^ self size

%


set class RubyCompilerStack
category: '*maglev-runtime'
method:
parentOrNil
  ^ self atOrNil: self size - 1

%


set class RubyCompilerStack
category: '(as yet unclassified)'
method:
pop
  | sz res |
  res := self at: (sz := self size).
  self size: (sz - 1) .
  ^ res

%


set class RubyCompilerStack
category: '(as yet unclassified)'
method:
pop: expectedTosObj
  | sz res |
  res := self at: (sz := self size).
  res == expectedTosObj ifFalse:[ self error:'corrupt compiler stack'].
  self size: (sz - 1) .
  ^ res

%


set class RubyCompilerStack
category: '(as yet unclassified)'
method:
push: anObject
  self addLast: anObject

%


set class RubyCompilerStack
category: '(as yet unclassified)'
method:
topOrNil

  ^ self atOrNil: self size 

%


set class RubyCompilerStack
category: '(as yet unclassified)'
method:
topValue
  ^ self at:( self size)

%

