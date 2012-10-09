(as yet unclassified)
argNodes
  | val |
  walked := true .
  ^ (val := valueNode) ifNil:[ #() ] ifNotNil:[ { val } ] 

