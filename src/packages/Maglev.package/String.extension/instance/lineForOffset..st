*maglev-debugging
lineForOffset: aByteOffset
  "returns a one-based line number corresponding to the
   1-based byte offset."
  | line lf |
  line := 1 .
  lf := Character lf .
  1 to: self size do:[:j |
    j >= aByteOffset ifTrue:[ ^ line ].
    (self at: j) == lf ifTrue:[ line := line + 1 ].
  ].
  ^ line

