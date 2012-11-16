as yet unclassified
getClearIter
  | res |
  res := iterNode . 
  res ifNotNil:[ iterNode := nil ].
  ^ res

