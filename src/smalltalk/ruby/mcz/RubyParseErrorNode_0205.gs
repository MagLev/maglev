
doit
RubyNode subclass: 'RubyParseErrorNode'
	instVarNames: #( message)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyParseErrorNode
removeallmethods
removeallclassmethods

set class RubyParseErrorNode
category: 'as yet unclassified'
method:
message
  ^ message

%


set class RubyParseErrorNode
category: 'as yet unclassified'
method:
message: aString
  message := aString

%

