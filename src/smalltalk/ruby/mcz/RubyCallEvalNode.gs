
doit
RubyCallNode subclass: 'RubyCallEvalNode'
	instVarNames: #( lexPathVar)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyCallEvalNode
removeallmethods
removeallclassmethods

set class RubyCallEvalNode
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


set class RubyCallEvalNode
category: '*maglev-runtime'
method:
hasBlockArg 
  ^ true "gets an implicit block, if no iter node"

%


set class RubyCallEvalNode
category: '*maglev-runtime'
method:
hasExplicitBlockArg
  ^ iterNode ~~ nil

%


set class RubyCallEvalNode
category: 'as yet unclassified'
method:
irArgNodes
  | res |
  res := super irArgNodes .
  iterNode ifNil:[ res add: self irImplicitBlockArg ].
  ^ res

%


set class RubyCallEvalNode
category: '*maglev-runtime'
method:
irNode 
  | asgn |
  (asgn := GsComAssignmentNode _basicNew) 
      dest: lexPathVar irLeaf  
      source: self irEvalLexPathLiteral .
  ^ GsComStatementsNode new list: { asgn . super irNode }.

%


set class RubyCallEvalNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  |  lpv |
  lpv := self walkEvalCall: aScope .
  lexPathVar := lpv .
  lpv walkWithScope: aScope .
  ^ super walkWithScope: aScope 

%

