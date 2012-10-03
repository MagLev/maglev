*maglev-ast
s_a: left b: nam_tok
  | res |
  (res := self _basicNew ) 
    leftNode: left  ;
    name: nam_tok symval ;
    position: nam_tok src_offset .
  ^ res

