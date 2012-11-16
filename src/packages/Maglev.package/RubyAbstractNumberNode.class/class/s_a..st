*maglev-ast
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

