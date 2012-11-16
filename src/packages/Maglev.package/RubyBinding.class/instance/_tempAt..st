*maglev-runtime
_tempAt: aSymbol
  "runtime access for LocalVarNode in an eval,
    accessing a temp not in the VariableContext "
  ^ tmpsDict ifNotNil:[ :dict | dict at: aSymbol otherwise: nil ]

