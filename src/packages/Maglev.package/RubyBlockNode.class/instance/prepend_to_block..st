*maglev-ast
prepend_to_block: aNode
  aNode == self ifTrue:[ self error:'recursive block B'].
  list insertAll: { aNode } at: 1  . 
  ^ self

