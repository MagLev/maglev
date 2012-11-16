*maglev-runtime
_lastArgTypeIsCallback
  | types sz |
  sz := (types := argTypes) size.
  sz ~~ 0 ifTrue:[ ^ ( types at: sz) isKindOfClass: CCallin ].
  ^ false

