*maglev-ast
colon2: left name: name_tok
  | c2n res |
  c2n := RubyColon2Node s_a: left b: name_tok .
  (res := self _basicNew)
     constNode: c2n ;
     position: name_tok src_offset . "valueNode is nil"
  ^ res

