*maglev-ast
append_to_block: aNode
  aNode == self ifTrue:[ self error:'recursive block A'].
  list add: aNode .
  ^ self

