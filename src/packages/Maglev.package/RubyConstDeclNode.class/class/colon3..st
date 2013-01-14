*maglev-ast
colon3: name_tok
  | c3n res |
  c3n := RubyColon3Node s_a: name_tok .
  (res := self _basicNew)
     constNode: c3n ;
     position: name_tok src_offset . "valueNode is nil"
  ^ res

