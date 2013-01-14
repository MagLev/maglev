*maglev-runtime
includesTemp: aSymbol
  "called from C code in parser prim 903"
  | dict bnd |
  (varLocs at: aSymbol otherwise: nil) ifNotNil:[ 
     ^ true 
  ].
  (bnd := theBinding) ifNotNil:[ 
    ^ (bnd locationForExistingName: aSymbol) ~~ nil
  ].
  ^ false

