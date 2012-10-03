*maglev-runtime
addRubyClassVar: aSymbol value: aValue env: envId
    " Returns aValue"
  | assoc tns  |
  aSymbol _isSymbol ifFalse:[ ArgumentTypeError signal:'name of a class variable must be a Symbol' ].
  (aSymbol at:1 equals:'@@') ifFalse:[ NameError signal:'class variable must start with @@'].
  tns := self _rubyClassVarNameSpaceForStore: aSymbol env: envId .
       "(SessionTemps current at:#TrapCV otherwise: false) ifTrue:[ self pause ]."
  ^ tns at: aSymbol classVarPut: aValue .
 


