
set class RubyAbstractLiteralNode
category: 'as yet unclassified'
method:
determineDynamic
  ^ nil  "thisNode :: X    signals TypeError at runtime"

%


set class RubyAbstractLiteralNode
category: 'as yet unclassified'
method:
irNode
	^ self ir: (GsComLiteralNode new leaf: self irLeaf)

%


set class RubyAbstractLiteralNode
category: 'as yet unclassified'
method:
__determineDynamic
 
  ^ nil  "thisNode :: X    signals TypeError at runtime"

%

