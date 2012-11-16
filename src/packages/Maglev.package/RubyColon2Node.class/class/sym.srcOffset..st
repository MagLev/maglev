*maglev-ast
sym: sym srcOffset: ofs
  | res |
  (res := self _basicNew) 
     name: sym ; position: ofs .
  ^ res

