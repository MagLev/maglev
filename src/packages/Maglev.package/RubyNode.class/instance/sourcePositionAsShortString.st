as yet unclassified
sourcePositionAsShortString
  | pos |
  (pos := position) ifNil:[ ^ 'unknown source position'].
  pos _isInteger ifTrue:[
	 ^ self sourcePositionAsString .  "RubyParser"
  ].
  ^ position asShortString "SimpleSourcePosition from MRI parse server"

