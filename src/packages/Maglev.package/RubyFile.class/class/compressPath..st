*maglev-runtime
compressPath: aPath
  "Compress '.' and '..' in aPath.  Assumes aPath is absolute (starts with '/').
  Removes trailing '/'."
  |  offsets pathSiz ofs idx prev newPath components cSize  |
  pathSiz := aPath size .
  (aPath indexOf: $. startingAt:1) == 0 ifTrue:[ | psiz |
     "no  .  found in path"
     (aPath at: pathSiz ) == $/ ifTrue:[
       ^ aPath copyFrom: 1 to: psiz - 1 .
     ].  
     ^ aPath   
  ].
  idx := 1 .
  offsets := { } .
  [ idx <= pathSiz ] whileTrue:[
     ofs := aPath indexOf: $/ startingAt: idx . 
     ofs ~~ 0 ifTrue:[
       offsets add: ofs .
       idx := ofs + 1 .
     ] ifFalse:[
       idx := pathSiz + 1 "trigger loop exit"
     ].
  ].
  offsets add: pathSiz + 1 .
  prev := offsets at: 1 .
  components := { } .
  newPath := String new .
  prev > 1 ifTrue:[ components add: (aPath copyFrom: 1 to: prev - 1 ) ] 
           ifFalse:[ newPath add: $/ "input starts with / " ].
  2 to: offsets size do:[:n |
    ofs := offsets at: n.
    ofs > (prev + 1) ifTrue:[
      (ofs == (prev + 3) and:[ (aPath at: prev + 1 equals:'..')]) ifTrue:[
	  "found .. , remove last component added" 
          (cSize := components size) > 0 ifTrue:[ components size: cSize - 1 ].
      ] ifFalse:[
	(ofs == (prev + 2) and:[ (aPath at: prev + 1) == $. ]) ifFalse:[
	  "not a single . "
	  components add:( aPath copyFrom: prev + 1 to: ofs - 1 ) .
	]
      ].
    ]. "ifFalse  skip repeated / "
    prev := ofs
  ].
  cSize := components size . 
  cSize > 0 ifTrue:[
    newPath add:( components at: 1 ) .
    2 to: cSize do:[:m | newPath add: $/ ; add:( components at: m )].
  ].
  ^ newPath .

