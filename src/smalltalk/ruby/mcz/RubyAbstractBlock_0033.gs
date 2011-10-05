
set class RubyAbstractBlock class
category: 'as yet unclassified'
method:
comment
^ 'a RubyAbstractBlock is an object pushed on the loopStack only.
  It does not represent any AST or IR node . '

%


set class RubyAbstractBlock
category: 'as yet unclassified'
method:
labelBreak
  ^ nil

%


set class RubyAbstractBlock
category: 'as yet unclassified'
method:
labelNext
  ^ nil

%


set class RubyAbstractBlock
category: 'as yet unclassified'
method:
labelRedo
  ^ labelRedo

%


set class RubyAbstractBlock
category: 'as yet unclassified'
method:
labelRedo: aGsComLabelNode
  ^ labelRedo := aGsComLabelNode

%

