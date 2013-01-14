as yet unclassified
pathToString: aPath
  | res delim pSize |
  (pSize := aPath size) == 0 ifTrue:[ ^ '' ].
  (res := String new) addAll: (aPath at: 1).
  delim := self delim  .
  2 to: pSize do:[:j | 
    res addAll: delim ; addAll: (aPath at:j)
  ].
  ^ res

