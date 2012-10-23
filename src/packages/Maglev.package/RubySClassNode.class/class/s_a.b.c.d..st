*maglev-runtime
s_a: rcv b: body c: srcOfs d: lineNum
  | res |
  (res := self _basicNew)
    receiverNode: rcv ; 
    bodyNode: body ;
    lineBias: 0   ;
    position: srcOfs ; startLine: lineNum .
  ^ res

