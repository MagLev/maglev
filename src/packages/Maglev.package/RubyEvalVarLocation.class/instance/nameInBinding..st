*maglev-runtime
nameInBinding: aSymbol
  vcOfs ifNotNil:[ self error:'vcOffset already assigned'].
  tmpName := aSymbol .

