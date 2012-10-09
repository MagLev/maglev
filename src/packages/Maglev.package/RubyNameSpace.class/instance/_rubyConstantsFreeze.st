*maglev-runtime
_rubyConstantsFreeze
  | pns |
  self associationsDo:[ :assoc |
    assoc isDefined ifNotNil:[ assoc _freezeConstant ]
  ].
  

