
doit
RubyNode subclass: 'RubyCaseNode'
	instVarNames: #( caseBody caseNode evalTmp)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyCaseNode
removeallmethods
removeallclassmethods

set class RubyCaseNode class
category: '*maglev-ast'
method:
s_a: exprArg b: body  c: ofs
  "body is a WhenNode  "
  | node  |
  (node := self _basicNew )
     caseNode: exprArg caseBody: body  ;
     position: ofs .
  ^ node

%


set class RubyCaseNode
category: 'accessing'
method:
caseBody

	 ^ caseBody

%


set class RubyCaseNode
category: 'accessing'
method:
caseBody: aNode
	caseBody := aNode

%


set class RubyCaseNode
category: 'accessing'
method:
caseNode

	 ^ caseNode

%


set class RubyCaseNode
category: 'accessing'
method:
caseNode: aNode
	caseNode := aNode

%


set class RubyCaseNode
category: '*maglev-ast'
method:
caseNode: nodeA caseBody: nodeB
  caseNode := nodeA .
  caseBody := nodeB

%


set class RubyCaseNode
category: '*maglev-runtime'
method:
irNode
  "we are handling            case target ; when ... ; when ... ; else ... ; end 
                 or           case        ; when ... ; when ... ; else ... ; end
      when there is no  target, we use nil rather than an evaluation tmp 
      self.caseNode  represents   target  "
  |  node  | 
  evalTmp ifNotNil:[ |leaf asgn bdy  |
       asgn := GsComAssignmentNode _basicNew dest: (leaf := evalTmp leaf)
                            source: caseNode irEvaluatedBlockNode  .
       self ir: asgn .
        bdy := caseBody irCaseNodeWithLeaf: leaf .
        node := GsComStatementsNode new list: { asgn . bdy }.
  ] ifNil:[
       node := caseBody irCaseNodeWithLeaf: nil 
  ].
   ^ node

%


set class RubyCaseNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
   
    caseBody walkWithScope: aScope .
    caseNode ifNotNil:[
	  evalTmp := aScope newEvaluationTemp .
	  caseNode walkWithScope: aScope
	 ].

%


set class RubyCaseNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:case, ', caseNode _inspect, ', ', caseBody _inspect , $]

%

