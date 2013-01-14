*maglev-runtime
s_a: nameSym b: args c: body
  | node |
  ( node := self _basicNew )
     nameNode:  nameSym  ;
     lineBias: 0 ;
     argsNode: args ;
     bodyNode: body .
   ^ node

