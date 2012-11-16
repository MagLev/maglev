*maglev-runtime
padded: direction to: length withString: padString

"Pad self with characters from padString to fill out to length.  Repeats padString as much as necessary."

| oldSize numPadChars  |

length <= (oldSize := self size) ifTrue:[ ^ self ].

"grow the receiver"
numPadChars := length - oldSize.
self size: length.
padString size == 0 ifTrue:[ self error:'pad string empty']. "TODO correct Ruby exception"
direction == #left ifTrue:[
  self fill: numPadChars with: padString startingAt: (oldSize + 1).
  ^self 
].
direction == #right ifTrue:[
  oldSize ~~ 0 ifTrue:[ 
    self replaceFrom: numPadChars + 1 to: numPadChars + oldSize with: self startingAt: 1
  ].
  self fill: numPadChars with: padString startingAt: 1.
  ^ self 
].
direction == #center ifTrue:[|half lSize rSize|
  half := numPadChars / 2 .
  lSize := half floor.
  rSize := half ceiling.
  oldSize ~~ 0 ifTrue:[ 
    self replaceFrom: lSize + 1 to: lSize + oldSize with: self startingAt: 1 .
  ].
  self fill: lSize with: padString startingAt: 1.
  self fill: rSize with: padString startingAt: (lSize + oldSize + 1).
  ^ self
].
ArgumentError signal:'unknown direction'.

