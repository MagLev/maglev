*maglev-runtime
locationForExistingName: aSymbol depth: aNumber

  (aSymbol == #'$~' or:[ aSymbol == #'$_' or:[ aSymbol == #__lexPath ]]) ifFalse:[
    | loc ofs dict |
    (loc := varLocs at: aSymbol otherwise: nil ) ifNotNil:[ 
      ^ loc 
    ].
    theBinding ifNotNil:[ :bnd| 
      ofs := bnd locationForExistingName: aSymbol .
      ofs ifNotNil:[
        loc := RubyEvalVarLocation _basicNew .
        ofs _isSmallInteger ifTrue:[ loc vcOffset: ofs ] 
                           ifFalse:[ loc nameInBinding: aSymbol ].
        varLocs at: aSymbol put: loc . 
        ^ loc
      ].
    ].
  ].
  ^ nil   

