*maglev-runtime
codePointIsRubyWhitespace
  "receiver should be result of aString>>codePointAt: "
    self <= 32 ifTrue:[
      self == 32 ifTrue: [^true].    "space"
      self == 10 ifTrue: [^true].    "line feed"
      self == 13 ifTrue: [^true].    "cr"
      self == 9 ifTrue: [^true].    "tab"
      self == 11 ifTrue:[ ^ true ].   "VT"
      self == 12 ifTrue: [^true].    "form feed"
    ].
    ^false

