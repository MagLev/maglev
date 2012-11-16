as yet unclassified
irAssignmentNode: srcVarNode 
  | node |
  (node := GsComAssignmentNode _basicNew)
       dest: self irLeaf source:  srcVarNode .
  ^ self ir: node 

