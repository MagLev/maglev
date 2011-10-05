
doit
RubyAbstractLiteralNode subclass: 'RubyStLiteralNode'
	instVarNames: #( value)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyStLiteralNode
removeallmethods
removeallclassmethods

set class RubyStLiteralNode
category: 'as yet unclassified'
method:
irNode
  ^ GsComLiteralNode newObject: value

%


set class RubyStLiteralNode
category: 'as yet unclassified'
method:
literalObject: anObject
  value := anObject

%

