as yet unclassified
asString
  | lineStr fileStr |
  lineStr := line ifNil:[ 'unknown' ] ifNotNil:[ line asString ].
  fileStr := filename ifNil:[ 'unknown file'] ifNotNil:[ filename ].
  ^ 'line ' , lineStr , ' of ' , fileStr

