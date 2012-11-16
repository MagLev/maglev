*maglev-debugging
lineForOffset: aByteOffset
  "returns a one-based line number corresponding to the
   1-based byte offset."
  source ifNotNil:[ ^ source lineForOffset: aByteOffset].
  ^ -1

