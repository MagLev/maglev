*maglev-ast
s_a: regex b: rcvr
  | res |
  (res := self _basicNew)
     receiverNode: rcvr ; valueNode: regex .
   ^ res

