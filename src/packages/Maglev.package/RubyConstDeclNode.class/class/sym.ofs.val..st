*maglev-ast
sym: sym ofs: srcOfs val: val
  | c2n res |
  (c2n := RubyColon2Node _basicNew) 
     name: sym ; position: srcOfs .
  (res := self _basicNew) 
     constNode: c2n ; valueNode: val  ; position: srcOfs .
  ^ res

