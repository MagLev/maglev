*maglev-runtime
rubySqueeze
 "return a copy of self with each repeating character sequence 
  reduced to a single character"
| n dest prev ch sz res |
sz := self size .
sz > 1 ifTrue:[
  res := self species new: sz // 2 .
  prev := -1 .
  n := 1 .
  dest := 0 .
  [ n <= sz and:[(ch := self at: n) ~~ prev]] whileTrue:[
    dest := dest + 1 .
    res at: dest put: ch .
    n := n + 1 .
    prev := ch
  ].
  [ n <= sz ] whileTrue:[ 
    (ch := self at: n) == prev ifTrue:[
      n := n + 1 .
      [ n <= sz and:[ (ch := self at: n) == prev ]] whileTrue:[
	n := n + 1
      ].
      n <= sz ifTrue:[
        dest := dest + 1 .
        res at: dest put: ch .
      ] 
    ] ifFalse:[
      dest := dest + 1 .
      res at: dest put: ch .
    ].
    n := n + 1 .
    prev := ch .
  ].
  dest < sz ifTrue:[ res size: dest ].
] ifFalse:[
  res := self copy .
].
^ res

