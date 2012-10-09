*maglev-runtime
rubyConstDefined: aSymbol 
  "a ruby primitive. "
  |  rns assoc envId |
  (rns := self nameSpace: (envId := 1"__callerEnvId") ) ifNotNil: [ 
    assoc := rns resolveConstant: aSymbol  .
    assoc ifNotNil:[ ^ assoc isDefined ].
  ].
  self == Object ifTrue:[ | cls |
    cls := self rubySuperclass: envId .
    assoc := self rubyConstAssociationAtOrNil: aSymbol env: envId .
    assoc ifNotNil:[ ^ assoc isDefined ].
  ].
  ^ false 

"ruby 1.9 code:   | assoc |
  assoc := self rubyConstAssociationAtOrNil: aSymbol 
        env: 1""__callerEnvId""  . 
  assoc ifNotNil:[ ^ assoc isDefined ]. 
  ^ false 
"

