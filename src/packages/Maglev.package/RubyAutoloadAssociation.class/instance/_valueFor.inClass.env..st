*maglev-runtime
_valueFor: aSymbol inClass: aModule env: envId 
  | assoc |
  assoc := self _assocFor: aSymbol inClass: aModule env: envId error: true .
  assoc isDefined ifNil:[ 
     NameError signal:'uninitialized constant ', key , ' from Autoload...>>_valueFor:'
  ].
  ^ assoc _valueNoAction

