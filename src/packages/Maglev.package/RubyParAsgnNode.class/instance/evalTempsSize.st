as yet unclassified
evalTempsSize
  | r |
  ^ (r := rightList) ifNil:[ 1 ] ifNotNil:[ r size ] .

