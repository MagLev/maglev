(as yet unclassified)
childrenForMatch
  | res |
  (res := super childrenForMatch) addLast: constNode .
  ^ res

