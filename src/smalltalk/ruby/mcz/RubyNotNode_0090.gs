
set class RubyNotNode
category: '*maglev-ast'
classmethod:
s_a: cond
   | res |
  (res := self _basicNew )
     conditionNode: cond .
  ^ res

%


set class RubyNotNode
category: 'accessing'
method:
conditionNode
  "a ruby primitive, in env 2"
   ^ conditionNode

%


set class RubyNotNode
category: 'accessing'
method:
conditionNode: aNode
	conditionNode := aNode

%


set class RubyNotNode
category: '*maglev-runtime'
method:
postWalkForYield
  conditionNode postWalkForYield

%


set class RubyNotNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		nextPutAll: '!';
		parenthesize: conditionNode

%


set class RubyNotNode
category: 'converting'
method:
receiverNode
	^ conditionNode

%


set class RubyNotNode
category: 'converting'
method:
selector
	^ #_not

%


set class RubyNotNode
category: '*maglev-runtime'
method:
_inspect
 ^ '[:not, ', conditionNode _inspect , $]

%

