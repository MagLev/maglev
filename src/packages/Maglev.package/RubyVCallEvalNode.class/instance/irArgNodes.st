as yet unclassified
irArgNodes
  | res |
  res := super irArgNodes .
  res add: self irImplicitBlockArg .
  ^ res

