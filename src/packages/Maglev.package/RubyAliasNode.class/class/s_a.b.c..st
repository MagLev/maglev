*maglev-ast
s_a: objA b: objB  c: srcOffsetSi
  | res |
  (res := self _basicNew ) 
     newName: objA oldName: objB ;
     position: srcOffsetSi .
  ^ res

