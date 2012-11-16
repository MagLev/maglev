*maglev-runtime
newEvaluationTemp
  | nam idx vInfo tNames ofs vnams |
  idx := 1 .
  tNames := #( aTmp1 aTmp2 aTmp3 aTmp4 aTmp5 aTmp6 ) .
  [ true ] whileTrue:[
    nam := tNames atOrNil: idx .
    nam ifNil:[ nam := ( ':aTmp' , idx asString ) asSymbol  ] . 
    (vInfo := self _findVariable:  nam) ifNil:[  
       ^ self _addVarInfo: RubyScopeVarInfo name: nam kind: #evaluationTmp
    ].
    idx := idx + 1
  ].

