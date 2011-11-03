
set class RubyFCallEvalNode
category: '*maglev-runtime'
method:
argNodes
    | itr args |
    args := argsNode fCallArgNodes .
    self evalAddFileLineArgs: args for: callName .
    (itr := iterNode) ifNotNil:[
      (args := args copy) add: itr .
    ].
    ^ args

%


set class RubyFCallEvalNode
category: 'as yet unclassified'
method:
hasBlockArg
  ^ true

%


set class RubyFCallEvalNode
category: '*maglev-runtime'
method:
hasExplicitBlockArg
  ^ iterNode ~~ nil

%


set class RubyFCallEvalNode
category: 'as yet unclassified'
method:
irArgNodes
  | res |
  res := super irArgNodes .
  iterNode ifNil:[ res add: self irImplicitBlockArg ].
  ^ res

%


set class RubyFCallEvalNode
category: '*maglev-runtime'
method:
irNode 
  | asgn lp |
  (asgn := GsComAssignmentNode _basicNew) 
      dest: lexPathVar irLeaf  
      source: self irEvalLexPathLiteral .
  ^ GsComStatementsNode new list: { asgn . super irNode }.

%


set class RubyFCallEvalNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  |  lpv |
  lpv := self walkEvalCall: aScope .
  lexPathVar := lpv .
  lpv walkWithScope: aScope .
  ^ super walkWithScope: aScope 

%

