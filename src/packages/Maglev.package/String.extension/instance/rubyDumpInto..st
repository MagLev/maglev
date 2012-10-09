*maglev-runtime
rubyDumpInto: result
| tbl lastSpecialVal vArr |
tbl := #( '\000' '\001' '\002' '\003' '\004' '\005' '\006' '\a'   "000 .. 007"
          '\b'   '\t'    '\n'   '\v'   '\f'    '\r'   '\016' '\017' "010 .. 017"
          '\020' '\021' '\022' '\023' '\024' '\025' '\026' '\027' "020 .. 027"
          '\030' '\031' '\032' '\e'   '\034' '\035' '\036' '\037'  "030 .. 037"
          ' '     '!'    '\"'    '\#').                              "040 .. 043"
vArr := { nil }.
"The dump output must be wrapped in double quotes"
result addLast: $" .
lastSpecialVal := tbl size - 1.
1 to: self size do:[ :n | | ch v  |
  ch := self at: n .
  v := ch asciiValue .
  v <= lastSpecialVal ifTrue:[
    result addAll: (tbl at: (v + 1))
  ] ifFalse:[
    v >= 127 ifTrue:[
	  vArr at: 1 put: v .
      result addLast: (Module sprintf: '\%o' with: vArr ) 
    ] ifFalse:[
      result addLast: ch .
    ].
  ].
].
result addLast: $".
^ result

