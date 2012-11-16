*maglev-cextensions
cextGlobalVarAt: stringArg  put: aValue
  | assoc aString |
  aString := stringArg .
  aString size == 0 ifTrue:[ ^ nil ].  "rb_gv_set() checks for size 0"
  (aString at: 1) == $$ ifFalse:[
    aString := '$' , aString
  ]. 
  assoc := Object rubyGlobalVarAssoc: aString asSymbol env: 1 .
  assoc globalVarValue: aValue .
  ^ aValue

