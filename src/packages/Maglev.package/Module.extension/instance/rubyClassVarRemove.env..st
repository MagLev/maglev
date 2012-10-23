*maglev-runtime
rubyClassVarRemove: aSymbol env: envId
  " Returns value of the removed  class var "
  | assoc  ns |
  aSymbol _isSymbol ifFalse:[ ArgumentTypeError signal:'name of a class variable must be a Symbol' ].
  (aSymbol at:1 equals:'@@') ifFalse:[ NameError signal:'class variable must start with @@'].
  assoc := self _rubyClassVarAssociation: aSymbol env: envId .
  assoc ifNil:[   
    NameError signal: 'class variable not found ' , aSymbol .
  ].
  ns := self _classForRubyClassVar transientNameSpaceForStore: envId .
  ns removeKey: aSymbol .
  ^ assoc _value

