*maglev-runtime
removeKey: aSymbol
  |  pm |
  moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
  pm := RubyCompilerState current persistenceMode .
  pm ifTrue:[
    self _removeKey: aSymbol ifAbsent:[] .
  ] ifFalse:[
    (self includesKey: aSymbol) ifTrue:[
         NameError signal:'cannot remove ' , aSymbol , 
        ' , persistenceMode==false and variable is persistent' 
    ].
  ].
 

