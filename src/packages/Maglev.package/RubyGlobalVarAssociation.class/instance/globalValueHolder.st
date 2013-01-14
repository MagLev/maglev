(as yet unclassified)
globalValueHolder 
  | arr |
  (arr := value) ifNil:[ arr := { nil } . value := arr  ].
  ^ arr

