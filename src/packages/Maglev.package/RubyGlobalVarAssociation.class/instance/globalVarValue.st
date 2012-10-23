(as yet unclassified)
globalVarValue
  "used to retrieve value of a Ruby global variable , such as with key  $a  .
   see also globalVarValue: "

  | arr |
  (arr := value) ifNil:[ ^ nil ] .
  ^ arr at: 1

