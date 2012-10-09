*maglev-ast
s_a: rcv b: asgnSelTok c: opSelTok d: val
  | res asgn_sel |
  asgn_sel := asgnSelTok symval .
  (res := self _basicNew )
     init_a: rcv b: asgnSelTok c: opSelTok d: val .
  ^ res

