*maglev-runtime
parent: aNameSpace
  
  "can't assert that Object's name space has parent == nil   because
      parent is installed before myClass when parsing.  Trac 607" 
 
  aNameSpace ifNotNil:[
    aNameSpace class == RubyNameSpace ifFalse:[
      self error:'invalid name space'
    ]
  ].
  parent == aNameSpace ifFalse:[ "avoid unnecessary write"
    parent := aNameSpace
  ]

