*maglev-runtime
asString
  | msg |
  [
    | args |
    (args := gsArgs) _isArray ifTrue:[ | obj str |
      obj := args atOrNil: 1 .
      str := args atOrNil: 2 .
      (obj is_aModule and:[ str _isOneByteString]) ifTrue:[ 
         "Print fully qualified name of a Ruby  Module"
         msg := 'The object ' , (obj rubyFullName: 1) ,
           ' may not be committed, ' , str .
      ].
    ]
  ] onException: Error do:[:ex | msg := nil ].
  ^ self _description: msg

