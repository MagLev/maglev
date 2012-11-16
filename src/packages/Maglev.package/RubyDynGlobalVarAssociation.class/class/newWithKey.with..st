as yet unclassified
newWithKey: aSymbol with: aBlock
  | assoc |
  assoc := self newWithKey: aSymbol .
  assoc block: aBlock .
  assoc setReadOnly .
  ^ assoc

