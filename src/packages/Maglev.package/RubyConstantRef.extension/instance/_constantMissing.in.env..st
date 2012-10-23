*maglev-runtime
_constantMissing: aSymbol in: aClass env: envId 
  | val assoc |
  val := aClass @ruby1:const_missing: aSymbol  .
  (assoc := RubySymbolAssociation newWithKey: aSymbol) _value: val .
  ^ assoc

