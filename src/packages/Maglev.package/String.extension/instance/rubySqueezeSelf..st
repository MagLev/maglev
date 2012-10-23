*maglev-runtime
rubySqueezeSelf: templates
 "A ruby primitive.
  Convert each repeating character sequence in receiver per templates
  to a single character"
| n dest prev ch sz charset |
charset := BitSet forTemplates: templates env: 1"__callerEnvId" .
sz := self size .
sz > 1 ifTrue:[
  prev := -1 .
  n := 1 .
  [ n <= sz and:[(ch := self codePointAt: n) ~~ prev]] whileTrue:[
    n := n + 1 .
    prev := ch
  ].
  dest := n - 1 .
  [ n <= sz ] whileTrue:[ 
    ((ch := self codePointAt: n) == prev and:[ (charset at: prev) == 1]) ifTrue:[
      n := n + 1 .
      [ n <= sz and:[ (ch := self codePointAt: n) == prev ]] whileTrue:[
        n := n + 1
      ].
      n <= sz ifTrue:[
        dest := dest + 1 .
        self codePointAt: dest put: ch .
      ] 
    ] ifFalse:[
      dest := dest + 1 .
      self codePointAt: dest put: ch .
    ].
    n := n + 1 .
    prev := ch .
  ].
  dest < sz ifTrue:[ self size: dest .  ^ self ]
           ifFalse:[ ^ nil " no changes made "]
] ifFalse:[
  ^ nil " no changes made"
]

