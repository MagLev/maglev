*maglev-ast
s_a: nam_tok
  | res |
  (res := self _basicNew ) 
    name: nam_tok symval ;
    position: nam_tok src_offset .
  ^ res

