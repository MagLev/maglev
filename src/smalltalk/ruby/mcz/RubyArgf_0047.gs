
doit
Object subclass: 'RubyArgf'
	instVarNames: #( argv stream lineno
	                  advance fileName closed)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #( instancesNonPersistent)

%

set class RubyArgf
removeallmethods
removeallclassmethods

set class RubyArgf class
category: '*maglev-runtime'
method:
with: anArgv
  ^ self _basicNew _init: anArgv

%


set class RubyArgf
category: '*maglev-runtime'
method:
_init: anArgv 
  (anArgv _isArray) ifFalse:[ ArgumentTypeError signal:'expected an Array'].
  argv := anArgv .
  stream := nil .
  lineno  := 0  .
  advance := true .
  closed := false .

%

