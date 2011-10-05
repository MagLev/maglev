
doit
RubyAbstractCallNode subclass: 'RubyGlobalNotAssignable'
	instVarNames: #( name msg valueNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalNotAssignable
removeallmethods
removeallclassmethods

set class RubyGlobalNotAssignable class
category: 'as yet unclassified'
method:
newForRp
  ^ self _basicNew

%


set class RubyGlobalNotAssignable
category: 'as yet unclassified'
method:
argNodes
  ^ { msg . valueNode }

%


set class RubyGlobalNotAssignable
category: 'as yet unclassified'
method:
irReceiverNode
  ^ GsComVariableNode globalNamed: #NameError inDict: Globals

%


set class RubyGlobalNotAssignable
category: 'as yet unclassified'
method:
isSmalltalkSend
  ^ true

%


set class RubyGlobalNotAssignable
category: 'as yet unclassified'
method:
name: aName 
  name := aName

%


set class RubyGlobalNotAssignable
category: '*maglev-ast'
method:
node_assign_set_rhs: rhs
  valueNode ifNil:[
    rhs is_void_result ifTrue:[
       RubyParserM signalError: 'void value expression'
    ].
    valueNode := rhs .
  ] ifNotNil:[
    RubyParserM signalError: ' value already assigned'
  ].
  ^ self       

%


set class RubyGlobalNotAssignable
category: 'as yet unclassified'
method:
selector
  ^ #signal:ignoring:

%


set class RubyGlobalNotAssignable
category: 'as yet unclassified'
method:
valueNode: aNode
  valueNode := aNode

%


set class RubyGlobalNotAssignable
category: '*maglev-ast'
method:
walkWithScope: aScope
    "Thread is mapped to GsProcess during ruby prims install" 
  (msg := RubyStrNode newForIr )
     _value: 'global ' , name , ' is a read-only variable' .
 
  ^ super walkWithScope: aScope

%


set class RubyGlobalNotAssignable
category: '*maglev-runtime'
method:
_inspect
  ^  '[:gasgnNotAssignable, :', name, ', ', valueNode _inspect , $]

%

