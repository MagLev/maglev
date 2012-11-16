*maglev-runtime
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

