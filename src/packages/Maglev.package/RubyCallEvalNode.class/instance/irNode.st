*maglev-runtime
irNode 
  | asgn |
  (asgn := GsComAssignmentNode _basicNew) 
      dest: lexPathVar irLeaf  
      source: self irEvalLexPathLiteral .
  ^ GsComStatementsNode new list: { asgn . super irNode }.

