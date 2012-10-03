*maglev-runtime
newVarLocation: aSymbol
  "returns a RubyEvalVarLocation"
  (aSymbol == #'$~' or:[ aSymbol == #'$_' or:[ aSymbol == #__lexPath ]]) ifFalse:[
    | loc |
    (varLocs at: aSymbol otherwise: nil) ifNotNil:[
      self error: 'duplicate newVarLocation'
    ].
    (loc := RubyEvalVarLocation _basicNew)  
       nameInBinding: aSymbol .
    varLocs at: aSymbol put: loc .
    ^ loc
  ].
  ^ nil

