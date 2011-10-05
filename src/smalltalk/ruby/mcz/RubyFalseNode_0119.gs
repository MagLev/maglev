
set class RubyFalseNode
category: 'as yet unclassified'
method:
definedQkind
  ^ #'false'

%


set class RubyFalseNode
category: 'as yet unclassified'
method:
irLeaf
	^ self ir: (GsComLitLeaf new specialLiteral: false)

%


set class RubyFalseNode
category: '*maglev-runtime'
method:
_inspect
  ^  ':false'

%

