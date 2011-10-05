
doit
Object subclass: 'RubyVisibility'
	instVarNames: #( restore)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyVisibility
removeallmethods
removeallclassmethods

set class RubyVisibility
category: 'accessing'
method:
restore

	 ^ restore

%


set class RubyVisibility
category: 'accessing'
method:
restore: a
	restore := a

%

