*maglev-runtime
terminatedWith: aCharacter
  | sz |
  (sz := self size) == 0 ifTrue:[ ^ '/' copy ].
  (self at: sz) == aCharacter ifTrue:[ ^ self ]
                      ifFalse:[ ^ self , aCharacter ]

