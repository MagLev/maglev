*maglev-runtime
isDigitsForInteger
  "Returns true if receiver is all digits 
   or has a leading minus sign and all digits"
  | idx |
  idx := 1 .
  ( self at: idx) == $- ifTrue:[ idx := idx + 1] .
  idx to: self size do:[:n |
	(self at: n) isDigit ifFalse:[ ^ false ].
  ].
  ^ true

