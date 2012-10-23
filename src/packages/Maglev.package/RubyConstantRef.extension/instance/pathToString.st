*maglev-runtime
pathToString
  | idx last str |
  idx := lexPathSize + 1 .
  last := self size  .
  str := String withAll: (self at: idx) .
  idx := idx + 1 .
  [ idx <= last ] whileTrue:[
    str add:  '::' ; add: (self at: idx) .
    idx := idx + 1 .
  ].
  ^ str

