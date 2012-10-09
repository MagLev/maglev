as yet unclassified
asShortString
  | lineStr fileStr |
  lineStr := line ifNil:[ 'unknown' ] ifNotNil:[ line asString ].
  fileStr := filename ifNil:[ 'unknown file'] ifNotNil:[ filename ].
  ^ 'line ' , lineStr , ' of ' , (RubyFile pathForTrace: fileStr)

