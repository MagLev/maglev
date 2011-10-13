
set class String
category: '*maglev-runtime'
method:
asRegexpWithOptions: aNumber
	^ Regexp new: self options: aNumber

%


set class String
category: '*maglev-runtime'
method:
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

%


set class String
category: '*maglev-runtime'
method:
isDigitsForInteger
  "Returns true if receiver is all digits 
   or has a leading minus sign and all digits"
  | idx |
  idx := 1 .
  ( self at: idx) == $- ifTrue:[ idx := idx + 1] .
  idx to: self size do:[:n |
	(self at: n) isDigit ifFalse:[ ^ false ].
  ].
  ^ true

%


set class String
category: '*maglev-debugging'
method:
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

%


set class String
category: '*maglev-runtime'
method:
md5sumDigest
  ^ self md5sum _asHexDigest

%


set class String
category: '*maglev-runtime'
method:
md5sumHexString
  ^ self md5sum asHexString

%


set class String
category: '*maglev-runtime'
method:
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

%


set class String
category: '*maglev-runtime'
method:
prefixIfRubySelector
      "ruby_selector_suffix dependent, and send sites"
  (self _rubyAt1: -4) == 35"$#" ifTrue:[ ^ self rubySelectorPrefixSymbol ].
  ^ self

%


set class String
category: '*maglev-runtime'
method:
rubyCapitalize

"Return a copy of self with all but the first lower case"
|result|
self size == 0 ifTrue:[ ^ self species new ].
result := self asLowercase.
result at: 1 put: ((self at: 1) asUppercase).
^ result.

%


set class String
category: '*maglev-runtime'
method:
rubyCount: templates
  "A ruby primitive.
   Return a count of characters in the receiver as specified by templates."
  |characters count| 
  characters := BitSet forTemplates: templates env: 1"__callerEnvId" .
  count := 0.
  1 to: self size do:[ :i |
    ( characters at: (self codePointAt: i)) == 1  ifTrue:[ count := count +  1 ]
  ].
  ^ count.

%


set class String
category: '*maglev-runtime'
method:
rubyDelete: templates
  "a ruby primitive.
   Return a copy of the receiver, minus those characters specified by templates"
  | characters result resIdx  |
  characters := BitSet forTemplates: templates "bits == 1 are chars to delete"
                        env: 1"__callerEnvId" .
  result := self species new .
  resIdx := 1 .
  1 to: self size do:[ :i | | ch |
    ch := self codePointAt: i .
    (characters at:  ch  ) == 0 ifTrue:[ 
      result codePointAt: resIdx put: ch . 
      resIdx := resIdx + 1 
    ]
  ].
  ^ result.

%


set class String
category: '*maglev-runtime'
method:
rubyDeleteInPlace: templates
  "A ruby primitive.
   Delete characters specified by templates from receiver."
  | characters destIdx recSize |
  characters := BitSet forTemplates: templates "bits == 1 are chars to delete"
                        env: 1"__callerEnvId" .
  destIdx := 0 .
  1 to: (recSize := self size) do:[ :n | 
    (characters at:  (self codePointAt: n )) == 1 ifTrue:[ " found first deletion point"
      destIdx := n - 1 .
       n + 1 to: recSize do:[ :k | | ch |
          ch := self codePointAt: k . 
          (characters at: ch ) == 0 ifTrue:[
              destIdx := destIdx + 1 .
              self codePointAt: destIdx put: ch 
          ].
       ].
       self size: destIdx . 
       ^ self 
    ]
  ].
  ^ nil  "no deletions found"

%


set class String
category: '*maglev-runtime'
method:
rubyDowncaseInPlace
	"Ensure all characters in self are lowercase. Return nil if no change, otherwise return self."
	| changed |
	changed := false .
	1 to: self size do:[ :i | | ch |
		ch := self at: i .
		ch isUppercase ifTrue:[ 
			changed := true .
			self at: i put: ch asLowercase ]].
	changed ifTrue:[ ^ self ] ifFalse:[ ^ nil ].

%


set class String
category: '*maglev-runtime'
method:
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

%


set class String
category: '*maglev-runtime'
method:
rubyFindLastAlphaNumeric
 "return the index of the last alpha numeric character in receiver.
  return nil if none."
|idx|
idx := self size.
[idx >=1 ] whileTrue: [
	(self at: idx) rubyIsAlphaNumeric ifTrue: [ ^ idx ] .
	idx := idx - 1 .
].
^ nil .

%


set class String
category: '*maglev-runtime'
method:
rubyLiteralClass
	(self at: 1) == $/
		ifTrue: [ ^ RubyRegexpNode ].
		
	^ (self includesSubString: '..' )
		ifTrue:[ RubyDotNode ]
		ifFalse:[ RubyAbstractNumberNode ]

%


set class String
category: '*maglev-runtime'
method:
rubySelectorSuffix
    "Return a String containing the suffix of self"
      "ruby_selector_suffix dependent"
  | res |
  res := self _rubyAt1: -4 length: 4 .
  res ifNotNil:[
    (res at: 1) == $# ifTrue:[ ^ res ].
  ].
  Error signal:'missing ruby selector suffix'

%


set class String
category: '*maglev-runtime'
method:
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

%


set class String
category: '*maglev-runtime'
method:
rubySqueeze: templates
 "A Ruby primitive.
  return a copy of receiver , converting each repeating character sequence per templates
  to a single character."
| res n dest prev ch sz charset |
charset := BitSet forTemplates: templates env: 1"__callerEnvId" .
sz := self size .
sz > 1 ifTrue:[
  res := self species new: sz // 2 .
  prev := -1 .
  n := 1 .
  dest := 0 .
  [ n <= sz and:[(ch := self codePointAt: n) ~~ prev]] whileTrue:[
    dest := dest + 1 .
    res codePointAt: dest put: ch .
    n := n + 1 .
    prev := ch
  ].
  [ n <= sz ] whileTrue:[ 
    ((ch := self codePointAt: n) == prev and:[ (charset at: prev) == 1]) ifTrue:[
      n := n + 1 .
      [ n <= sz and:[ (ch := self codePointAt: n) == prev ]] whileTrue:[
    n := n + 1
      ].
      n <= sz ifTrue:[
        dest := dest + 1 .
        res codePointAt: dest put: ch .
      ] 
    ] ifFalse:[
      dest := dest + 1 .
      res codePointAt: dest put: ch .
    ].
    n := n + 1 .
    prev := ch .
  ].
  dest < sz ifTrue:[ res size: dest ].
] ifFalse:[
  res := self copy .
].
^ res

%


set class String
category: '*maglev-runtime'
method:
rubySqueezeSelf
 "convert each repeating character sequence in receiver to a single character"
| n dest prev ch sz |
sz := self size .
sz > 1 ifTrue:[
  prev := -1 .
  n := 1 .
  [ n <= sz and:[(ch := self at: n) ~~ prev]] whileTrue:[
    n := n + 1 .
    prev := ch
  ].
  dest := n - 1 .
  [ n <= sz ] whileTrue:[ 
    (ch := self at: n) == prev ifTrue:[
      n := n + 1 .
      [ n <= sz and:[ (ch := self at: n) == prev ]] whileTrue:[
	n := n + 1
      ].
      n <= sz ifTrue:[
        dest := dest + 1 .
        self at: dest put: ch .
      ] 
    ] ifFalse:[
      dest := dest + 1 .
      self at: dest put: ch .
    ].
    n := n + 1 .
    prev := ch .
  ].
  dest < sz ifTrue:[ self size: dest .   ^ self ]
           ifFalse:[ ^ nil "no changes made" ]
] ifFalse:[
  ^ nil "no changes made"
].

%


set class String
category: '*maglev-runtime'
method:
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

%


set class String
category: '*maglev-runtime'
method:
rubySucc
"Implements  Ruby   succ!  for String "
| idx res carry sz lastAlphaNumericIdx |

sz := self size .
sz == 0 ifTrue: [^ self ] .

lastAlphaNumericIdx := self rubyFindLastAlphaNumeric .
lastAlphaNumericIdx == nil 
	ifTrue: [ 
		"String has no alpha numerics, so increment everything"
		idx := sz . 
		[ idx >= 1 ] whileTrue:[
			carry := self _rubyIncrementCharAt: idx .
  			carry == nil ifTrue:[ ^ self ] .
  			idx := idx - 1 .
		].
	] ifFalse: [|firstAlphaNumericIdx|
		"String has alphanumerics, so increment starting at rightmost alpha numeric"
		firstAlphaNumericIdx := lastAlphaNumericIdx .
		idx := lastAlphaNumericIdx .
		[ idx >= 1 ] whileTrue:[
		     (self at: idx) rubyIsAlphaNumeric ifTrue: [
			  firstAlphaNumericIdx := idx .
			  carry := self _rubyIncrementCharAt: idx .
  			  carry == nil ifTrue:[ ^ self ] 
			] .
  			idx := idx - 1
		].
		carry ~~ nil ifTrue:[ self insertAll: carry at: firstAlphaNumericIdx ] .
		^ self .
	] .
carry ~~ nil ifTrue: [ self insert: carry at: 1 ] .
^ self

%


set class String
category: '*maglev-runtime'
method:
rubySwapcaseInPlace

"Return a copy of self with all characters swap cased.  Return nil if receiver unchanged.
  Only works on ascii range a-z and A-Z.  Locale ignorant."
|modified|
modified := false .
1 to: self size do: [ :i ||ch ascii|
	ch := self at: i .
	ascii := ch asciiValue .
	(ascii >= 65 and:[ ascii <= 90] ) ifTrue: [
		modified := true .
		self at: i put: ch asLowercase 
	].
	(ascii >= 97 and:[ ascii <= 122] ) ifTrue: [
		modified := true .
		self at: i put: ch asUppercase
	] .
] .
modified ifTrue:[ ^ self ] ifFalse: [^ nil ] .

%


set class String
category: '*maglev-runtime'
method:
rubyTrFrom: from to: to

"Translate characters in self.  from is a set of characters to translate (possibly negated). to
is a set of corresponding characters to translate to.  The first character in from is changed
to the first character in to, and so on.  If there are more characters in from than to, extend to
with its last character.  Modifies self and returns self, unless no changes made, and then returns nil."

| map idx changed selfsiz |

( (selfsiz := self size) == 0 or:[ from size == 0]) ifTrue:[ ^ nil].

changed := false .
map := from _trMapping: to.

"A nil in the map means to remove that character from the string."
idx := 0 .
1 to: selfsiz do:[:i| | current replacement |
	current := ((self codePointAt: i) + 1) . "Characters are zero based"
	replacement := map at: current .
	replacement ifNotNil:[ 
		idx := idx + 1. 
		((self at: idx) == replacement) ifFalse: [ changed := true ] .
		self at: idx put: replacement.
	] .
].

idx ~~ (selfsiz ) ifTrue: [ self size: idx ] .
changed ifTrue: [ ^ self ] ifFalse: [ ^ nil ] .

%


set class String
category: '*maglev-runtime'
method:
rubyTrSqueezeFrom: from to: to

"Like rubyTrFrom:to:, but removes duplicate characters from modified sections of self.
Returns self, or nil if self not modified."

|map modSet modified writeIndex prevModChar|
(self size = 0 or:[ from size = 0]) ifTrue:[ ^ self ].

map := from _trMapping: to .
modified := false .
modSet := BitSet forTemplateString: from .
prevModChar := nil .
writeIndex := 1 .
1 to: self size do:[:i| | currentByte |
	currentByte := self codePointAt: i .
	(modSet at: currentByte) = 1 
		ifTrue:[ | newChar |
			modified := true .
			newChar := map at: (currentByte + 1) .
			(newChar ~~ prevModChar) 
				 ifTrue:[
					"This is not a repeated character, so modify"
					self at: writeIndex put: newChar .
					writeIndex := writeIndex + 1 .
					prevModChar := newChar]]
		ifFalse:[
			prevModChar := nil .
			writeIndex ~~ i ifTrue:[ self at: writeIndex put: (self at: i) ] .
			writeIndex := writeIndex + 1 ]].
modified ifTrue:[ self size: (writeIndex - 1) . ^ self ] ifFalse:[ ^ nil ].

%


set class String
category: '*maglev-runtime'
method:
rubyUpcaseInPlace
	"Ensure all characters in self are uppercase. Return nil if no change, otherwise return self."
	| changed |
	changed := false .
	1 to: self size do:[ :i | | ch |
		ch := self at: i .
		ch isLowercase ifTrue:[ 
			changed := true .
			self at: i put: ch asUppercase ]].
	changed ifTrue:[ ^ self ] ifFalse:[ ^ nil ].

%


set class String
category: '*maglev-runtime'
method:
rubyUpperCaseAt: anIndex
  "anIndex is one-based. "
  
  ^ (self at: anIndex) asUppercase asciiValue


%


set class String
category: '*maglev-runtime'
method:
sequenceExpand

^ self sequenceExpand: false.

%


set class String
category: '*maglev-runtime'
method:
sequenceExpand: skipFirst

"Return a copy of self with all sequences expanded.  A sequence is two characters separated by $-, e.g., a-z.
If skipFirst is true, then skip the first character."

| expanded index limit rcvrSize chCls  |

expanded := String new.
(rcvrSize := self size) == 0 ifTrue:[ ^ expanded ].

index :=  skipFirst ifTrue:[ 2 ] ifFalse:[ 1].
limit := rcvrSize - 2 .
chCls := Character .
[ index <= limit  ] whileTrue:[
	(self at: index + 1) == $- 
	  ifTrue:[ "expand character sequence: e.g., a-z"
		(self codePointAt: index) to: (self codePointAt: (index + 2))
		     do:[:i| expanded add: (chCls withValue: i)].
		index := index + 2 ]
	ifFalse:[ expanded add: (self at: index) ].
	index := index + 1 
] .
[ index <= rcvrSize ] whileTrue:[
	expanded add: (self at: index) .
	index := index + 1 
] .
^ expanded.

%


set class String
category: '*maglev-runtime'
method:
suffixIfRubySelector
      "ruby_selector_suffix dependent, and send sites"
  (self _rubyAt1: -4) == 35"$#" ifTrue:[ ^ self rubySelectorSuffix ].
  ^ nil

%


set class String
category: '*maglev-runtime'
method:
tailForPrint: aSize
  self size > aSize ifTrue:[ ^ '...', (self last: aSize) ] 
	              ifFalse:[ ^ self ].

%


set class String
category: '*maglev-runtime'
method:
terminatedWith: aCharacter
  | sz |
  (sz := self size) == 0 ifTrue:[ ^ '/' copy ].
  (self at: sz) == aCharacter ifTrue:[ ^ self ]
                      ifFalse:[ ^ self , aCharacter ]

%


set class String
category: '*maglev-runtime'
method:
_inspect
  ^ '"' , self, $"

%


set class String
category: '*maglev-runtime'
method:
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

%


set class String
category: '*maglev-runtime'
method:
_rubyLstrip

"Returns a new String containing the same Characters as the receiver,
 but with leading whitespace separators removed."

| sz |
(sz := self size) == 0 ifTrue:[ ^ self copy ].
(self codePointAt: 1) codePointIsRubyWhitespace ifFalse: [ ^ self copy ].
2 to: sz do:[:j |
  (self codePointAt: j) codePointIsRubyWhitespace ifFalse:[ 
    ^ self copyFrom: j to: sz
  ].
].
^ self class new

%


set class String
category: '*maglev-runtime'
method:
_rubyLstripInPlace

"delete leading whitespace separators from receiver"
| sz |
(sz := self size) == 0 ifTrue:[ ^ nil ].
(self codePointAt: 1) codePointIsRubyWhitespace ifFalse: [ ^ nil ].
2 to: sz do:[:j |
  (self codePointAt: j) codePointIsRubyWhitespace ifFalse:[
     self removeFrom: 1 to: j - 1 .
     ^ self.
  ].
].
self size: 0 . 
^ nil

%


set class String
category: '*maglev-runtime'
method:
_rubyRstrip

"Returns a new String containing the same Characters as the receiver,
 but with trailing whitespace or NUL removed."

| sz cp |

(sz := self size) == 0 ifTrue: [ ^ self copy ].
((cp := self codePointAt: sz ) == 0 or:[ cp codePointIsRubyWhitespace ]) ifFalse:[
  ^ self copy 
].
sz - 1 _downTo: 1 do:[ :j |
  (self codePointAt: j) codePointIsRubyWhitespace ifFalse:[ "for 1.9 add   or:[ cp == 0 ]"
    ^ self copyFrom: 1 to: j 
  ].
].
^ self class new

%


set class String
category: '*maglev-runtime'
method:
_rubyRstripInPlace
 "delete trailing whitespace or NUL from receiver."
| sz cp |
(sz := self size) == 0 ifTrue: [ ^ nil ].
((cp := self codePointAt: sz ) == 0 or:[ cp codePointIsRubyWhitespace ]) ifFalse:[
  ^ nil 
].
sz - 1 _downTo: 1 do:[ :j |
  (self codePointAt: j) codePointIsRubyWhitespace ifFalse:[ "for 1.9 add   or:[ cp == 0 ]"
     self removeFrom: j + 1 to: sz . 
     ^ self 
  ].
].
self size: 0 .
^ self

%


set class String
category: '*maglev-runtime'
method:
_rubyStrip
| first limit sz cp |
((sz := self size) == 0) ifTrue: [
   ^ self copy
].
limit := sz + 1.
first := 1 .
(self codePointAt: 1) codePointIsRubyWhitespace ifTrue: [ | j |
  first := nil .
  j := 2.
  [ j == limit ] whileFalse: [
      (self codePointAt: j) codePointIsRubyWhitespace ifTrue: [
         j := j + 1.
      ] ifFalse:[
         first := j.
         j := limit .
       ].
  ].
  first ifNil: [ ^ self class new ].
].

((cp := self codePointAt: sz ) == 0 or:[ cp codePointIsRubyWhitespace ]) ifTrue:[
  sz - 1 _downTo: 1 do:[ :k |
    (self codePointAt: k) codePointIsRubyWhitespace ifFalse:[ "for 1.9 add   or:[ cp == 0 ]"
      ^ self copyFrom: first to: k
    ].
  ].
].
first == 1 ifTrue:[  ^ self copy ].
^ self copyFrom: first to: sz .

%


set class String
category: '*maglev-runtime'
method:
_rubyStripInPlace
| first limit sz cp newSiz |
((sz := self size) == 0) ifTrue: [
   ^ nil "no modification made"
].
limit := sz + 1.
first := 1 .
(self codePointAt: 1) codePointIsRubyWhitespace ifTrue: [ | j |
  first := nil .
  j := 2.
  [ j == limit ] whileFalse: [
      (self codePointAt: j) codePointIsRubyWhitespace ifTrue: [
         j := j + 1.
      ] ifFalse:[
         first := j.
         j := limit .
       ].
  ].
  first ifNil: [ self size: 0 . ^ self ].
].

((cp := self codePointAt: sz) == 0 or:[ cp codePointIsRubyWhitespace]) ifTrue: [
  sz - 1 _downTo: 1 do:[ :k |
    (self codePointAt: k) codePointIsRubyWhitespace ifFalse:[ "for 1.9 add   or:[ cp == 0 ]"
       self replaceFrom: 1 to: (newSiz := 1 + k - first) with: self startingAt: first .
       self size: newSiz .
       ^ self
    ].
  ].
].
first ~~ 1 ifTrue:[  
  newSiz := 1 + sz - first .
  self replaceFrom: 1 to: newSiz with: self startingAt: first .
  self size: newSiz .
  ^ self
].
^ nil "no modification made"

%


set class String
category: '*maglev-runtime'
method:
_trMapping: to

"Create a translation map for tr.  Characters in self are mapped to characters in the to string.
If self starts with $^, then the sense is negated. Character ranges (e.g., 'a-z') are expanded.
A nil in the map means to delete that character. If self is the single character $^, then treat
as non-negating."

| map expandedTo expandedFrom defaultCh negating toSiz |

negating := (self size > 1) and: [ (self at: 1) == $^] . "Handle self is '^'"
expandedTo := to sequenceExpand .
expandedFrom := self sequenceExpand: negating. "Note: this will not have the leading $^."
toSiz := expandedTo size .
defaultCh := toSiz == 0 ifTrue:[ nil ] ifFalse: [expandedTo at: toSiz] .

negating
	ifTrue:[
		"We are negating the characters in from.  All characters not in from get
		set to the default character."
		map := Array new: 256 withAll: defaultCh .
		1 to: expandedFrom size do:[:i| | index | 
			index := (expandedFrom codePointAt: i) + 1 .
			"Set all the characters we are not translating back to themsleves."
			map at: index put: (expandedFrom at: i)]]
	ifFalse:[ |  frSiz n index limit chCls  | 
		map := Array new: 256 .
		chCls := Character .
		1 to: 256 do:[:i| map at: i put: (chCls withValue: (i-1)) ] .
		frSiz := expandedFrom size .
		limit := toSiz min: frSiz .
		n := 1 .
		[ n <= limit ] whileTrue:[ | toCh |
		   index := (expandedFrom codePointAt: n) + 1 .
		   toCh := expandedTo at: n .
		   map at: index put: toCh .
		   n := n + 1 ].
	    [ n <= frSiz ] whileTrue:[
			index := (expandedFrom codePointAt: n) + 1 .
			map at: index put: defaultCh .
			n := n + 1 ] .
		].
^ map.

%

