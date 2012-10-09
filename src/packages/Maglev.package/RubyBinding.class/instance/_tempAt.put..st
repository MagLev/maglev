*maglev-runtime
_tempAt: aSymbol put: aValue
  "runtime store for LocalAsgnNode in an eval,
    to a temp created after the VariableContext was defined.
   Returns aValue. "
  | dict |
  (dict := tmpsDict) ifNil:[ 
    dict := IdentityKeyValueDictionary new .
    tmpsDict := dict
  ].
  dict at: aSymbol put: aValue .
  ^ aValue

