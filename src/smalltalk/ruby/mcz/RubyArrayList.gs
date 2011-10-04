
doit
Object subclass: 'RubyArrayList'
	instVarNames: #( size)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyArrayList
removeallmethods
removeallclassmethods

set class RubyArrayList
category: 'accessing'
method:
size

	 ^ size

%


set class RubyArrayList
category: 'accessing'
method:
size: a
	size := a

%

