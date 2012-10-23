*maglev-runtime
addRubyInstVar: aSymbol

  "Adds a new instance variable named aSymbol to the receiver. 
 caller must have gotten true from  isModifiable_oops_notModule "

| offset ivn |
aSymbol _isSymbol ifFalse:[ ArgumentTypeError signal:'instVar name is not a Symbol'].
(ivn := instVarNames)  .
( ivn includesIdentical: aSymbol ) ifTrue:[
    ^ self _error: #rtErrAddDupInstvar args:{ aSymbol }
].
self _incrementInstVars: 1 .
offset := self instSize .  "the offset of the new instance variable"
ivn == #() ifTrue:[ 
  ivn := { } .  instVarNames := ivn .
].
ivn insertObject: aSymbol at: offset .
constraints == #() ifFalse:[ constraints insertObject: Object  at: offset ].
self _refreshClassCache: false .
  "no recompilation of methods nor handling of subclasses needed"
^ self

