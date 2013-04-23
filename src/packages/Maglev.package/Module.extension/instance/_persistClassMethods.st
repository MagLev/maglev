*maglev-runtime
_persistClassMethod
  | klass |
  klass := self class .
  (klass ~~ nil and: [klass ~~ Module]) ifTrue: [
    klass _persistMethods
  ]