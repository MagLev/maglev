(as yet unclassified)
pop
  | sz res |
  res := self at: (sz := self size).
  self size: (sz - 1) .
  ^ res

