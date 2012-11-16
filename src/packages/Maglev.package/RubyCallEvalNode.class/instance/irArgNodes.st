as yet unclassified
irArgNodes
  | res |
  res := super irArgNodes .
  iterNode ifNil:[ res add: self irImplicitBlockArg ].
  ^ res

