*maglev-runtime
newTempLeafNamed: aSymbol
  | res |
  (res := GsComVarLeaf new)
			blockTemp: aSymbol
			sourceLexLevel: self lexLevel.
  TraceLocals >= 1 ifTrue:[
	 self trace: ' tempLeafNamed: ', aSymbol , ' , created varLeaf blockTemp'
  ].
  ^ res 

