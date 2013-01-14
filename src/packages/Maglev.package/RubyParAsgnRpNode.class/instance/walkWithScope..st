*maglev-ast
walkWithScope: aScope
  | rpNode node cls  |
  rpNode := self .
  cls := self class .
  node := rpNode walkRpNode .  "does conversion and become"
  node class == cls ifTrue:[ self error:'failed to convert ParAsgnRp to ParAsgn'].
  node walkWithScope: aScope .

