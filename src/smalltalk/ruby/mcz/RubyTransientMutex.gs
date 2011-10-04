
doit
TransientMutex subclass: 'RubyTransientMutex'
	instVarNames: #( owner)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #( dbTransient)

%

set class RubyTransientMutex
removeallmethods
removeallclassmethods
