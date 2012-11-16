(as yet unclassified)
argNodes
  | t e  |
  t := thenBody .
  e := elseBody .
  ^ t ifNil:[
      e ifNil:[ #() ] ifNotNil:[ { e } ]
    ] ifNotNil:[
      e ifNil:[ { t } ] ifNotNil:[ { t . e } ]
    ]

