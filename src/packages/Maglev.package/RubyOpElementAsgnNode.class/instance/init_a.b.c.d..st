*maglev-ast
init_a: rcv b: arefArgs c: opSelTok d: val
  receiverNode := rcv .
  argsNode := arefArgs .
  callName := opSelTok symval .
  valueNode := val .
  self position: opSelTok src_offset 

