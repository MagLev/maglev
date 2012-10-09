*maglev-runtime
pathArray
 | arr |
 leftNode ~~ nil ifTrue:[
    (arr := leftNode pathArray) ifNil:[
      arr := { } .
    ] ifNotNil:[
      arr  addLast: name asSymbol
    ].
 ] ifFalse:[
    arr := { name asSymbol } .
 ] .
 ^ arr

