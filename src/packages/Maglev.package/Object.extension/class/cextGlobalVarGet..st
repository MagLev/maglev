*maglev-cextensions
cextGlobalVarGet: stringArg
  | sym assoc aString |
  aString := stringArg .
  aString size == 0 ifTrue:[ ^ nil ].
  (aString at: 1) == $$ ifFalse:[
    aString := '$' , aString 
  ].
  sym := Symbol _existingWithAll: aString .
  sym ifNil:[ ^ nil ].
  ^ (Object rubyGlobalVarAssoc: sym env: 1 ) globalVarValue

