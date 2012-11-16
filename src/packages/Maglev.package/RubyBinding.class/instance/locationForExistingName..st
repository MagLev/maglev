*maglev-runtime
locationForExistingName: aSymbol
  "compile time access , returns a SmallInteger , aSymbol, or nil"
  | d ofs |
  (d := names) ifNotNil:[
    (ofs := d at: aSymbol otherwise: nil) ifNotNil:[ 
      ^ ofs  "offset of the temp in the VariableContext"
    ]   
  ].
  (d := tmpsDict) ifNotNil:[
    (d at: aSymbol otherwise: nil) ifNotNil:[ 
      ^ aSymbol  "temp exists after dynamic addition"
    ]   
  ].
  ^ nil

