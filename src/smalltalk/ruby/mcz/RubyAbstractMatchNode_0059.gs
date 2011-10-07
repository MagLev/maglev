
set class RubyAbstractMatchNode
category: '*maglev-runtime'
method:
initImplicitDollarTildeForRp
  (implicitDollarTilde := RubyVcGlobalNode _basicNew) name: #'$~' 

%


set class RubyAbstractMatchNode
category: 'as yet unclassified'
method:
receiverNode
  ^ receiverNode

%


set class RubyAbstractMatchNode
category: 'as yet unclassified'
method:
receiverNode: aNode
	receiverNode := aNode

%


set class RubyAbstractMatchNode
category: 'as yet unclassified'
method:
selector
  ^ #=~

%


set class RubyAbstractMatchNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  implicitDollarTilde walkWithScope: aScope .
  ^ super walkWithScope: aScope 

%

