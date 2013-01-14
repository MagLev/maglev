*maglev-runtime
classOrNameSpaceAt: aSymbol inModule: aModule 
  | assoc val | 
  assoc := self resolveConstant: aSymbol .
  assoc ifNotNil:[  
    assoc := assoc _classFor: aSymbol inModule: aModule env: envId .   "may trigger autoload"
    assoc ifNil:[  "re-resolve after autoload"
      assoc := self resolveConstant: aSymbol .
    ].
    assoc ifNotNil:[ val := assoc _valueNoAction ]. "for boot, no check of isDefined"
  ].
  val ifNil:[ ^ nil ].
  val isNameSpace ifTrue:[   | cls |
    cls := val myClass .
    cls ifNil:[ ^ val "return a name space" ].
    cls isBehavior ifTrue:[ ^ cls ].
  ] ifFalse:[
    val isBehavior ifTrue:[ ^ val ] .
  ].
  ArgumentTypeError signal: 'value is not a Module or Class'

