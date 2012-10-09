(as yet unclassified)
pop: expectedTosObj
  | sz res |
  res := self at: (sz := self size).
  res == expectedTosObj ifFalse:[ self error:'corrupt compiler stack'].
  self size: (sz - 1) .
  ^ res

