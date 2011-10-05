
doit
RubyAbstractLiteralNode subclass: 'RubyAbstractNumberNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST-Abstract'
	options: #()

%

set class RubyAbstractNumberNode
removeallmethods
removeallclassmethods

set class RubyAbstractNumberNode class
category: '*maglev-ast'
method:
s_a: val
  | res |
  val _isInteger ifTrue:[  "handles both SmallInteger and LargeInteger"
    ^ RubyFixnumNode _basicNew _value: val
  ].
  val _isFloat ifTrue:[
    ^ RubyFloatNode _basicNew _value: val 
  ].
  val _isOneByteString ifTrue:[
     ^ self s_a: (self valueToNumber: val) .
  ].
  RubyParserM signalError: 'invalid arg to RubyAbstractNumberNode' .
  ^ nil

%


set class RubyAbstractNumberNode class
category: '*maglev-runtime'
method:
valueToNumber: aString

  aString isDigitsForInteger ifTrue:[
    ^ Integer fromString: aString
  ] ifFalse:[
    ^ Float fromString: aString
  ]

%

