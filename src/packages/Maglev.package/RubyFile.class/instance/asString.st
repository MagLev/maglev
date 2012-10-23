(as yet unclassified)
asString
 | res |
   "before 19aug08  asString was inherited"
 res := super asString .
 path ~~ nil ifTrue:[ res addAll: ': ...' ; addAll: (path last: (40 min: path size) )].
 ^ res

