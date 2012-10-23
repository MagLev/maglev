*maglev-ast
s_a: left b: right
  | res |
  (res := self _basicNew) 
    firstNode: left secondNode: right.
  ^ res

