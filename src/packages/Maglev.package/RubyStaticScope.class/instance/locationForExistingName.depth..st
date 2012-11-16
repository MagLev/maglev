*maglev-runtime
locationForExistingName: aName depth: aNumber
  | vAssoc res  |
  vAssoc := self _findVariable: aName .
  vAssoc ifNotNil:[
    (res := RubyVarLocation _basicNew) varInfo: vAssoc depth: aNumber scope: self .
    ^ res
  ] ifNil:[
    ^ enclosingScope ifNotNil:[
        enclosingScope locationForExistingName: aName depth: aNumber + 1  
    ]
  ]

