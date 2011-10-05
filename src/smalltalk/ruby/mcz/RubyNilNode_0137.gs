
doit
RubyAbstractLiteralNode subclass: 'RubyNilNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyNilNode
removeallmethods
removeallclassmethods

set class RubyNilNode class
category: 'parsetree'
method:
newForIr
   ^ self _basicNew  "during IR phase, for literals leave position nil"

%


set class RubyNilNode
category: 'as yet unclassified'
method:
definedQkind
  ^ #'nil'

%


set class RubyNilNode
category: 'as yet unclassified'
method:
determineDynamic
  " a global or constant name of form  nil::X  signals TypeError at runtime"

  ^ nil

%


set class RubyNilNode
category: 'as yet unclassified'
method:
irLeaf
	^ self ir: (GsComLitLeaf new specialLiteral: nil)

%


set class RubyNilNode
category: 'as yet unclassified'
method:
irYieldStarNode
  | n |
  (n := GsComArrayBuilderNode new)
     appendElement: (GsComLiteralNode newNil) .
  ^ n

%


set class RubyNilNode
category: 'as yet unclassified'
method:
isNilNode
  ^ true

%


set class RubyNilNode
category: 'as yet unclassified'
method:
pathArray
  " a global or constant name of form  nil::X  signals TypeError at runtime"

  ^ nil

%


set class RubyNilNode
category: 'as yet unclassified'
method:
printSourceOn: aStream
	aStream nextPutAll: 'nil'

%


set class RubyNilNode
category: '*maglev-runtime'
method:
_inspect
  ^  ':nil'

%

