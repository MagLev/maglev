
doit
Object subclass: 'RubyFlipFlop'
	instVarNames: #( theState)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyFlipFlop
removeallmethods
removeallclassmethods

set class RubyFlipFlop class
category: 'as yet unclassified'
method:
init: aFlipFlop
  ^ aFlipFlop ifNil:[ self _basicNew _init ]

%


set class RubyFlipFlop
category: 'as yet unclassified'
method:
_init
  theState := false

%

