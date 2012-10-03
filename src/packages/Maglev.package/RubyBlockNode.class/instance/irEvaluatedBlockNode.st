*maglev-runtime
irEvaluatedBlockNode
 " typical use:  IR generation for a Ruby begin..end  that does not contain a rescue keyword "
  | node lst |
  lst := { } .
  self irNodeListInto: lst .
  (node := GsComStatementsNode new) list: lst .
  self ir: node .
  ^ node 

