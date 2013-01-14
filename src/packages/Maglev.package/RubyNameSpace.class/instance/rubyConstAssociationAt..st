*maglev-runtime
rubyConstAssociationAt: aSymbol
  | anAssoc |
  (anAssoc := self resolveConstant: aSymbol ) ifNil:[
      ^ NameError signal:'constant ', aSymbol , ' , not found'
  ].
  ^ anAssoc

