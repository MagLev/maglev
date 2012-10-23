*maglev-runtime
fixedInstVars: arrayNode
  "Returns true if ok, false if fixed instVars already installed"
  fixedIvs ifNil:[ fixedIvs := arrayNode . ^ true ].
  ^ false

