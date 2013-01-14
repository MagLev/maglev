*maglev-ast
s_a: rcv b: arefArgs c: opSelTok d: val
  | res asgn_sel |
  asgn_sel := opSelTok symval .
  (res := self _basicNew )
     init_a: rcv b: arefArgs c: opSelTok d: val .
  ^ res

