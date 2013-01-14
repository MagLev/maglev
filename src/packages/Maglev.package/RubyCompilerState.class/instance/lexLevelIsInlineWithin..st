as yet unclassified
lexLevelIsInlineWithin: aValue
  "Return true if all lexical levels from current to aValue+1 represent in-line blocks"
  | idx idxLimit stk |
  stk := lexLevelStack .
  idx := stk depth .
  idxLimit := aValue + 2 . "first level is 0 , stored at stk at:1"
  [ idx >= idxLimit ] whileTrue:[ | v |
	(( v := stk at: idx) bitAnd:16rFFFF) < aValue ifTrue:[ self error:'inconsistent lexLev'].
     (v bitAnd: 16r10000) == 0 ifTrue:[ ^ false ] .
     idx := idx - 1 .
  ].  
  ^ true

