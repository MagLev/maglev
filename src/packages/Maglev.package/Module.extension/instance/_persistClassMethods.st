*maglev-runtime
_persistClassMethod
  | klass |
  klass := self class .
  klass == Module ifTrue: [
    self virtualClass _persistMethods.
  ] ifFalse: [
    (klass ~~ nil) ifTrue: [
      klass _persistMethods
    ]
  ]
