
set class RubyWhileNode
category: '*maglev-runtime'
classmethod:
selectorFor: conditionFirstBool
      "maybe ruby_selector_suffix dependent"
 
  ^ conditionFirstBool ifTrue:[ #whileTrue ] ifFalse:[ #untilFalse ]

%


set class RubyWhileNode
category: '(as yet unclassified)'
method:
argNodes
  | s |
  ^ (s := bodyNode ) ifNil:[ #() ] ifNotNil:[ { s } ]

%


set class RubyWhileNode
category: 'as yet unclassified'
method:
baseSelector
  ^ condIsFirst ifTrue:[ #whileTrue: ] ifFalse:[ #untilFalse: ] .
 
	

%


set class RubyWhileNode
category: 'as yet unclassified'
method:
irArgNodes
  ^ { self irBodyNode }

%


set class RubyWhileNode
category: 'as yet unclassified'
method:
irReceiverNode
	^ self irConditionNode

%


set class RubyWhileNode
category: 'converting'
method:
nameForPrint
  ^ 'while'

%


set class RubyWhileNode
category: 'as yet unclassified'
method:
receiverNode
  ^ conditionNode

%


set class RubyWhileNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:while, ', conditionNode _inspect, ', '
         , bodyNode _inspect, ', '
         , self baseSelector _inspect , $]

%

