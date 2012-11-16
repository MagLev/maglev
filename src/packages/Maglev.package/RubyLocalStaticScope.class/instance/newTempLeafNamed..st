*maglev-runtime
newTempLeafNamed: aSymbol
  | theLeaf |
  theLeaf :=  GsComVarLeaf new .
  (aSymbol == #'$~' or:[ aSymbol == #'$_' or:[ aSymbol == #'__lexPath']]) 
     ifTrue:[   theLeaf methVcGlobal: aSymbol ] 
    ifFalse:[  theLeaf methodTemp: aSymbol ].

  TraceLocals >= 1 ifTrue:[
    self trace: ' tempLeafNamed: ', aSymbol , ' , created methTemp leaf'
  ].
  ^  theLeaf

