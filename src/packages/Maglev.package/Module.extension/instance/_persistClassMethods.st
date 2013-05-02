*maglev-runtime
_persistClassMethod
  | klass |
  klass := self class .
  (klass == nil) ifFalse: [
    klass _persistMethods
  ]