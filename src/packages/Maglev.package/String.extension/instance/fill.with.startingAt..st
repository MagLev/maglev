*maglev-runtime
fill: length with: fillCharacters startingAt: start

"Copy characters from fillCharacters into self, startingAt start. Cycle through fillCharacters as necessary
to get to length characters.  '1234' fill: 2 with 'ABC' startingAt: 3 gives: '12AB'. 
'1234567890' fill: 5 with 'ABC' startingAt: 2 gives: '1ABCAB7890'.
Returns self."

"TODO: error checking"
| srcSize current leftToFill |
srcSize := fillCharacters size.
srcSize == 1 ifTrue:[ | ch idx |
  ch := fillCharacters at: 1 .
  start to: start + length - 1  do:[ :n |  self at: n put: ch ] .
] ifFalse:[
    current := start.
    1 to: (length // srcSize) do:[:i|
      self replaceFrom: current to: current + srcSize - 1 
		with: fillCharacters startingAt: 1 .
      current := current + srcSize
    ].
    leftToFill := length \\ srcSize.
    leftToFill >= 1 ifTrue:[
      self replaceFrom: current to: current + leftToFill - 1
	    with: fillCharacters startingAt: 1
    ].
].

