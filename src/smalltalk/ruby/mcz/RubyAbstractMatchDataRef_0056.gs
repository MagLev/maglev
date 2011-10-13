
set class RubyAbstractMatchDataRef
category: 'as yet unclassified'
method:
rcvr: aNode
  rcvrNode := aNode

%


set class RubyAbstractMatchDataRef
category: 'as yet unclassified'
method:
receiverNode
  "rcvrNode filled in by walkWithScope to be the class MatchData ,
   at runtime   selector   will resolve to a classmethod in MatchData  "

  ^ rcvrNode

%

