(as yet unclassified)
invariantGlobalVarValue: aValue
   | arr |
  (arr := value) ifNil:[ 
     arr := { aValue } .  
     value := arr       
  ] ifNotNil:[  
     arr at: 1 put: aValue
  ].
  isDefined := true .
  arr immediateInvariant .
  self immediateInvariant .
  ^ aValue 

