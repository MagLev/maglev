
doit
RubyNode subclass: 'RubyBlockPassNode'
	instVarNames: #( bodyNode inBoot)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyBlockPassNode
removeallmethods
removeallclassmethods

set class RubyBlockPassNode class
category: '*maglev-ast'
method:
s_a: body
  | res |
  (res := self _basicNew) bodyNode: body .
  ^ res

%


set class RubyBlockPassNode
category: 'accessing'
method:
bodyNode

	 ^ bodyNode

%


set class RubyBlockPassNode
category: 'accessing'
method:
bodyNode: aNode
	bodyNode := aNode

%


set class RubyBlockPassNode
category: 'as yet unclassified'
method:
childrenForMatch
	^ super childrenForMatch

%


set class RubyBlockPassNode
category: '*maglev-runtime'
method:
irNode
  ^ inBoot ifTrue:[ bodyNode irNode ]
          ifFalse:[ bodyNode irBlockPassNode ]

%


set class RubyBlockPassNode
category: 'as yet unclassified'
method:
isBlockPassNode
  ^ true

%


set class RubyBlockPassNode
category: '*maglev-runtime'
method:
postWalkForYield
  bodyNode postWalkForYield

%


set class RubyBlockPassNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
    inBoot := aScope inBootstrap  .
    bodyNode walkWithScope: aScope .

%


set class RubyBlockPassNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:block_pass, ', bodyNode _inspect , $]

%

