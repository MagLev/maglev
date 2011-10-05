
doit
Object subclass: 'RubyCallType'
	instVarNames: #( name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyCallType
removeallmethods
removeallclassmethods

set class RubyCallType
category: 'accessing'
method:
name

	 ^ name

%


set class RubyCallType
category: 'accessing'
method:
name: aString
	name := aString

%

