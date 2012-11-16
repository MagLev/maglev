*maglev-runtime
_rubyIncrementCharAt: idx
" returns nil if no carry, or a Character that represents
  the character to insert at start of result if idx==1 , per Ruby succ  method"
|  v |
v := self codePointAt: idx .
"$A is ascii 65  $Z is ascii 90"
(v >= 65 and:[ v <= 89])  ifTrue: [ self codePointAt: idx put: ( v + 1 ). ^ nil ] .
v == 90 ifTrue: [ self at: idx put: $A.  ^ $A] .

"$a is ascii 97  $z is ascii 122"
(v >= 97 and:[ v <= 121] ) ifTrue: [ self codePointAt: idx put: ( v + 1). ^ nil ] .
v == 122 ifTrue: [ self at: idx put: $a . ^$a ] .

"$0 is ascii 48  $9 is ascii 57"
(v >= 48 and:[ v <= 56] ) ifTrue: [ self codePointAt: idx put: (v + 1). ^ nil ] .
v == 57 ifTrue: [ self at: idx put: $0 . ^$1 ] .

v <= 254 ifTrue:[ self codePointAt: idx put: ( v + 1). ^ nil ]
       ifFalse:[  self codePointAt: idx put:  0 .
	            ^ Character withValue: 1  ] 

