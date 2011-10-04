
doit
RubyAbstractWhileNode subclass: 'RubyUntilNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyUntilNode
removeallmethods
removeallclassmethods

set class RubyUntilNode class
category: '*maglev-runtime'
method:
selectorFor: conditionFirstBool
      "maybe ruby_selector_suffix dependent"
  
  ^ conditionFirstBool ifTrue:[ #whileFalse ] ifFalse:[ #untilTrue ]

%


set class RubyUntilNode
category: '(as yet unclassified)'
method:
argNodes
  | s |
  ^ (s := conditionNode ) ifNil:[ #() ] ifNotNil:[ { s } ]

%


set class RubyUntilNode
category: 'as yet unclassified'
method:
baseSelector

  ^ condIsFirst ifTrue:[ #whileFalse: ] ifFalse:[ #untilTrue: ]

%


set class RubyUntilNode
category: 'as yet unclassified'
method:
irArgNodes
  
	^ {  blksInline ifTrue:[ self irConditionNode ]
		             ifFalse:[ self irBodyNode ] 
		 }

%


set class RubyUntilNode
category: 'as yet unclassified'
method:
irReceiverNode
  ^  blksInline ifTrue:[ self irBodyNode ]
                ifFalse:[ self irConditionNode ]

%


set class RubyUntilNode
category: 'converting'
method:
nameForPrint
  ^ 'until'

%


set class RubyUntilNode
category: 'as yet unclassified'
method:
receiverNode
  ^ bodyNode

%


set class RubyUntilNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:until, ', conditionNode _inspect, ', ' 
         , bodyNode _inspect, ', '
         , self baseSelector _inspect , $]

%

