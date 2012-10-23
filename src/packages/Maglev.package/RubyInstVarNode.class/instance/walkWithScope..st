as yet unclassified
walkWithScope: aScope
  | nam |
  nam := name.
  ( nam at: 1) == $@ ifFalse:[ self error:'instVar name does not start with @ '].
  (nam at: 2 equals:'_st_') ifTrue:[
    stName := ( nam copyFrom: 6 to: nam size) asSymbol 
  ].
  ^ super walkWithScope: aScope

