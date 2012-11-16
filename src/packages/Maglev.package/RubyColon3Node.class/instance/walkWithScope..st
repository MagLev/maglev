as yet unclassified
walkWithScope: aScope
  ^ self walkWithScope: aScope isDefinedQ: false .

  "A  Colon3 node accesses a constant explicitly from top scope, like  ::Array"  
  "no lexical children"

