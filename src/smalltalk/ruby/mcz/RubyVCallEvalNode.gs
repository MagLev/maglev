
set class RubyVCallEvalNode
category: '*maglev-runtime'
method:
argNodes
  "expect an ArgumentError at runtime, too few args,
   so don't bother to synthesize __FILE__ or __LINE__"
  ^ super argNodes

%


set class RubyVCallEvalNode
category: 'as yet unclassified'
method:
asCallNodeForIter
  | node |
  (node := RubyCallEvalNode _basicNew)
     position: position ;
     methodName: callName ;
     receiverNode: rcvrNode .
  ^ node

%


set class RubyVCallEvalNode
category: 'as yet unclassified'
method:
hasBlockArg
  ^ true

%


set class RubyVCallEvalNode
category: 'as yet unclassified'
method:
irArgNodes
  | res |
  res := super irArgNodes .
  res add: self irImplicitBlockArg .
  ^ res

%


set class RubyVCallEvalNode
category: '*maglev-runtime'
method:
irNode 
  | asgn lp |
  (asgn := GsComAssignmentNode _basicNew) 
      dest: lexPathVar irLeaf  
      source: self irEvalLexPathLiteral .
  ^ GsComStatementsNode new list: { asgn . super irNode }.

%


set class RubyVCallEvalNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  | lpv |
  lpv := self walkEvalCall: aScope .
  lexPathVar := lpv .
  lpv walkWithScope: aScope .
  ^ super walkWithScope: aScope 

%

