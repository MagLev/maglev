*maglev-runtime
s_a: args b: body c: srcOfs d: endOfs
  | res |
  (res := self _basicNew) 
     _varNode: args ; position: srcOfs ;
     bodyNode: body endOfs: endOfs .
  ^ res

