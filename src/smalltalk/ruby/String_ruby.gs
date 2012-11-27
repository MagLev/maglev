!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: String_ruby.gs 26189 2011-07-18 17:02:57Z otisa $
!
!=========================================================================

set class String

!  additional methods  for String to support Ruby

category: 'Ruby support'

classmethod:
new
"optimization, reimplement to hide squeak style Object(C)>>new ,
  no need for initialize on byte objects"

<primitive: 51>

self _primitiveFailed: #new .
%

classmethod:
_installRubyVariables
  "Install ruby class variables."
  | table sym |

  "This table is used to escape double quoted strings.  The table is
  indexed by the ASCII value of a character.  Any non-zero entries are
  output, preceeded by $\.  E.g., ASCII newlines (ASCII value 10) are
  printed as '\n', since $n is the value at index 10."

  sym := #RubyEscapeTranslationTable .
  (self _resolveClassVar: sym ) ifNil:[
    table := ByteArray new: 256 .
    table at:  7 + 1 put: $a asciiValue;
        at:  8 + 1 put: $b asciiValue;
        at:  9 + 1 put: $t asciiValue;
        at: 10 + 1 put: $n asciiValue;
        at: 11 + 1 put: $v asciiValue;
        at: 12 + 1 put: $f asciiValue;
        at: 13 + 1 put: $r asciiValue;
        at: 27 + 1 put: $e asciiValue;
        at: 34 + 1 put: $" asciiValue;
        at: 35 + 1 put: $# asciiValue;
        at: 92 + 1 put: $\ asciiValue .
    String _addInvariantClassVar: sym value: table .
  ].
%

run
String _installRubyVariables .
^ true
%

classmethod:
fromForRuby: anObjectForRuby
  "see the instance side >>forRuby. creates a String object from aString forRuby"
  ^ anObjectForRuby encodeAsUTF8 asByteArray asString
%

method:
_rubyFindLastSubString: subString startingAt: startIndex
  "adapt myself to subString and call the findLastSubString:startingAt: method"
  ^ (self forRubyAdaptedTo: subString) findLastSubString: subString forRuby startingAt: startIndex

%

method:
_rubyAddAll: anArg

  "A Ruby primitive.
   If anArg is a String, appends anArg to the receiver.
   else if anArg is a SmallInteger  >= 0 and <= 255,
   appends   Character withValue:anArg    to the receiver.
   Returns the receiver."

<primitive: 689>
anArg _isSmallInteger ifTrue:[
  ArgumentTypeError signal:'<<: ' , anArg asString , ' out of range' .
].
(anArg _isOneByteString) ifFalse:[
  anArg _error: #rtErrBadArgKind args:{ SmallInteger . String }.
].
self _primitiveFailed:#_rubyAddAll: args: { anArg }
%

! edits to _rubyAt1:  must be replicated to _rubyAt2: for env 2"
method:
_rubyAt1: anOffset
  "A ruby primitive.
   Ruby  aString[anIntOrRangeOrRegexp] for env 1

   If argument is an SmallInteger,
     anOffset is zero based.
     negative offsets go backwards from end,  -1 means last element.
     returns nil if anOffset is out of range,
     else returns character code at specified position.
  "
anOffset isInteger ifTrue: [ | offset |
  anOffset < 0 
    ifTrue:[ offset := anOffset + self _rubySize. ]
    ifFalse: [ offset := anOffset].
  (offset < 0 or:[offset >= self _rubySize]) ifTrue:[^ nil].
  ^ String fromForRuby: (self forRuby at: offset +1) asString 
].
anOffset _isOneByteString  ifTrue:[ "a String"  | ofs |
  ofs := self _findString: anOffset startingAt: 1 ignoreCase: false .
  ofs ~~ 0 ifTrue:[ ^ anOffset ].
  ^ nil.
].
anOffset _isRegexp ifTrue:[ "a Regexp" | aMatchData |
  aMatchData := anOffset match: self .
  aMatchData _storeRubyVcGlobal: 16r20 . "store into caller's $~ "
  aMatchData ~~ nil ifTrue:[  ^ aMatchData _rubyAt:0 ].
  ^ nil .
].
(anOffset isInterval) ifTrue:[
  ^ self _rubyAt1Interval: anOffset 
].
^ self @ruby1:__prim_at_failed: anOffset
%

method:
forRuby
  "use class-side fromForRuby: to create a String out of the returned object"
  ^ (Utf8 fromString: self) asUnicodeString
%

method:
forRubyAdaptedTo: aString
  "the result can contain all characters aString forRuby contains."
  | selfForRuby otherForRuby |
  selfForRuby := self forRuby.
  otherForRuby := aString forRuby.
  selfForRuby charSize < otherForRuby charSize 
    ifTrue:[^ otherForRuby class newFrom: selfForRuby].
  ^ selfForRuby
%

method:
_rubyAt1Interval: anInterval
  | size result |
  size := self _rubySize.
  anInterval begin > size ifTrue: [ ^ nil. ].
  "anInterval end < anInterval begin ifTrue: [^ String new]."
  result := String new.
  anInterval rubyDo: [ :index | 
    (self _rubyAt1: index)
      ifNil: [ ^ result ]
      ifNotNilDo: [ :newCharacter | 
        result := result,  newCharacter ].
  ] withLength: size.
  ^ result.
%

method:
_rubySize
  ^ self forRuby size
%
method:
_rubySize: anInteger
  ^ self size: anInteger
%


method:
_rubyReplaceFrom: start to: end with: aReplacement
  " replace from start to end including end "
  ^ self _rubyReplaceFrom: start limit: end + 1 with: aReplacement
%

method:
_rubyReplaceFrom: start limit: end with: aReplacement
  " replace from start to end excluding end "
  | selfForRuby aReplacementForRuby sizeOfTheResultForRuby aResultForRuby indexInResult |

  " check for valid arguments "
  start <= end ifFalse: [ OffsetError signal:'out of string: start <= end: ', start asString, ' <= ', end asString ].
  start >= 0 ifFalse: [ OffsetError signal:'out of string: start >= 0: ', start asString, ' >= 0' ].
  end <= self _rubySize ifFalse: [ OffsetError signal:'out of string: end <= self _rubySize: ', end asString, ' <= ', self _rubySize asString ].

  " convert ourselves for ruby "
  selfForRuby := self forRubyAdaptedTo: aReplacement.
  aReplacementForRuby := aReplacement forRuby.
  sizeOfTheResultForRuby := selfForRuby size + aReplacementForRuby size - end + start.
  aResultForRuby := selfForRuby class new: sizeOfTheResultForRuby.

  " start putting the result together 

  selfForRuby          [--------------|------------|------------------]
                       0              start        end                size
  aReplacementForRuby                 [------------------]

  aResultForRuby       [--------------|------------------|------------------]
                       0              start                                 sizeOfTheResultForRuby
  "
  indexInResult := 1 .
  1 to: start do: [ :indexInSelfForRuby | 
    aResultForRuby at: indexInResult put: (selfForRuby at: indexInSelfForRuby).
    indexInResult := indexInResult + 1 ].
  1 to: aReplacementForRuby size do: [ :indexInReplacementForRuby |
    aResultForRuby at: indexInResult put: (aReplacementForRuby at: indexInReplacementForRuby).
    indexInResult := indexInResult + 1 ].
  end + 1 to: selfForRuby size do: [ :indexInSelfForRuby | 
    aResultForRuby at: indexInResult put: (selfForRuby at: indexInSelfForRuby).
    indexInResult := indexInResult + 1 ].

  " claim the value from the result "
  self _rubyReplace: (String fromForRuby: aResultForRuby).
  ^ self
%

method:
_rubyByteAt: anOffset length: aCount

 "A ruby primitive.
  Returns an instance of receiver's class
   containing specified substring of the receiver,
   or returns nil if anOffset is out of range.
  Negative offsets go backwards from end,  -1 means last element.
  Used by Smalltalk code in MatchData .
"
<primitive: 687>
^ self _primitiveFailed: #_rubyAt:length: args: { anOffset . aCount }
%

method:
_rubyAt: anOffset length: aCount

 "A ruby primitive.
  Returns an instance of receiver's class
   containing specified substring of the receiver,
   or returns nil if anOffset is out of range.
  Negative offsets go backwards from end,  -1 means last element.
  Used by Smalltalk code in MatchData .
"
<primitive: 687>
^ self _primitiveFailed: #_rubyAt:length: args: { anOffset . aCount }
%

method:
_rubyByteAt: anOffset
  ^ (self at: anOffset) asInteger
%

method:
_rubyAt1: anOffset length: aCount

 "A ruby primitive.
  Returns an instance of receiver's class
   containing specified substring of the receiver,
   or returns nil if anOffset is out of range.
  Negative offsets go backwards from end,  -1 means last element.
  For env 1.
"
  anOffset _isRegexp ifTrue: [^ self _rubyAt1Regexp: anOffset length: aCount ].
  anOffset isInteger ifTrue: [^ self _rubyAt1Integer: anOffset length: aCount ].

%

method:
_rubyAt1Integer: anInteger length: aCount
  | aRange |
  aRange := self 
      _rubyRangeAt: anInteger 
      length: aCount 
      onNegativeOffsetDo: [ ^ nil ]
      onNegativeCountDo: [ ^ nil ].

  ^ self _rubyAt1: aRange
%

method:
_rubyAt1Regexp: anRegexp length: aCount
  |aMatchData |
  aMatchData := anRegexp match: self .
  aMatchData _storeRubyVcGlobal: 16r20 . "store into caller's $~ "
  aMatchData ~~ nil ifTrue:[  ^ aMatchData _rubyAt: aCount].
  ^ nil .
%

method:
_rubyAt1: anOffset length: aCount put: aString

  "If anOffset is an SmallInteger,  anOffset is zero based , and
   replace elements anOffset .. aCount-1 of receiver with contents of aString .

   If anOffset is a Regexp , replace the portion of the receiver
   specified by  (anOffset.match(self))[aCount] with aString ,
   where aCount is zero based.

   returns aString."
  | aRange |
  anOffset _isRegexp ifTrue: [ self error: 'This marvelous feature is not yet implemented. TODO!' ].

  aRange := self 
      _rubyRangeAt: anOffset 
      length: aCount 
      onNegativeOffsetDo: [OffsetError signal: 'index ', anOffset asString, ' out of string']
      onNegativeCountDo: [ OffsetError signal: 'negative length ', aCount asString].

  ^ self _rubyAt1: aRange put: aString
%

method:
_rubyRangeAt: anOffset length: aCount onNegativeOffsetDo: anOffsetBlock onNegativeCountDo: aCountBlock
  | positiveOffset |
  aCount < 0 ifTrue: aCountBlock.
  positiveOffset := anOffset < 0 
      ifTrue: [ anOffset + self _rubySize ]
      ifFalse: [ anOffset].
  positiveOffset < 0 ifTrue: anOffsetBlock.
  ^ (Range from: positiveOffset limit: positiveOffset + aCount).
%

method:
_rubyAt1: anOffset put: aValue
  "A ruby primitive.
   Ruby  [int]=  for env 1

   If anOffset is an SmallInteger and aValue is a SmallInteger ,
     (aValue bitAnd:255) replaces specified character of receiver, without auto-grow.
   If anOffset is an SmallInteger and aValue is a String, deletes character
   at anOffset, and then inserts aValue at anOffset .
   If both are Strings, replaces first occurrance of anOffset in receiver with aValue.

   Returns aValue
  "
  "<primitive: 690>"
  anOffset _isOneByteString ifTrue:[ ^ self _rubyAt1String: anOffset put: aValue ].
  anOffset isInteger ifTrue:[ ^ self _rubyAt1Integer: anOffset put: aValue ].
  anOffset _isRegexp ifTrue:[ ^ self _rubyAt1Regexp: anOffset put: aValue ].
  anOffset isInterval ifTrue:[ ^ self _rubyAt1Interval: anOffset put: aValue ].

  ^ self @ruby1:__prim_at_put_failed: anOffset _: aValue

%

method:
_rubyAt1Integer: anInteger put: aValue
  | offset charToReplace |
  offset := anInteger < 0 
    ifTrue:[ anInteger + self _rubySize ]
    ifFalse:[ anInteger ].
  ^ self _rubyReplaceFrom: offset to: offset with: aValue.

%

method:
_rubyAt1Interval: anInterval put: aReplacement
  ^ anInterval rubyReplaceIn: self with: aReplacement
%
method:
_rubyAt1String: aString put: aValue
  | offset |

  offset := self _findString: aString startingAt: 1 ignoreCase: false .
  offset == 0 ifTrue:[
     ^ OffsetError signal:'argument string not found'.
  ].
  ^ self _rubyAt1: offset - 1 length: aString size put: aValue
%

method:
_rubyAt1Regexp: aRegexp put: aValue
  |aMatchData mStart mLimit|
  aMatchData := aRegexp match: self  .
  aMatchData == nil ifTrue:[ ^ OffsetError signal:'argument regex not found'. ].
  mStart := aMatchData at: 1 . "mStart is zero based "
  mLimit := aMatchData at: 2 .
  ^ self _rubyAt1: mStart length: mLimit - mStart  put: aValue
%



method:
_rubyReplace: aString

  "Replace contents of receiver with contents of argument.
   Returns the receiver."

  aString ~~ self ifTrue:[ | argSize arg |
    (arg:= aString) _isOneByteString ifFalse:[ 
      "self __threadSaveCallerEnv ."
      arg := [:a | a @ruby1:to_str ] value: aString
          onSynchronous: Exception do: [:ex| nil ].
      arg _isOneByteString ifFalse:[
         ArgumentTypeError signal: 'in _rubyReplace:, to_str did not return a String'
      ].
    ].
    self size: (argSize := arg size) .
    argSize ~~ 0 ifTrue:[
      self replaceFrom: 1 to: argSize with: arg startingAt: 1 .
    ].
  ].
  ^ self
%

method:
isOctetString
  ^ true
%

method:
asOctetString
  ^ self
%

method:
rubyUnpack: aString
 "A ruby primitive.
  Per Pickaxe p 624 , table 27.14
  TODO: type in all the unpack documentation here."

<primitive: 722>
aString _isOneByteString ifFalse:[
  aString _error: #rtErrBadArgKind args:{ String }
].
self _primitiveFailed: #rubyUnpack: args: { aString }
%

method:
executeOnServer

  ^ System performOnServer: self
%


method:
_rubyInspect

"A ruby primitive.
  Returns a String whose contents are a displayable representation of the
 receiver."

"This reimplementation is for efficiency and to make the String | describe
 method more robust."

self size > 1000000 ifTrue:[
 ^ self
] ifFalse: [ | str |
  str := String new .
  str add: $" .
  self _rubyQuoteOn: str .
  str add: $" .
  
  ^ str
]
%

method:
_rubyQuoteOn: aString

"Puts a displayable representation of receiver on aStream escaping self
according to ruby double quote semantics (e.g., tab is replaced with '\n'
and $\ is escaped, etc). The characters that are translated:

  \a  ASCII 7   Bell
  \b  ASCII 8   Backspace
  \t  ASCII 9   Tab
  \n ASCII 10  New Line
  \v ASCII 11  Vertical Tab
  \f  ASCII 12  Form Feed
  \r ASCII 13  Carriage Return
  \e  ASCII 27 ESC

Characters that are quoted: Backslash $\ ASCII 92 and double quote ASCII 34"
 | charCls vArr |
  charCls := Character .
  vArr := { nil }.
  0 to: self _rubySize - 1 do: [:n | | c xlated av chDone |
    c := self _rubyAt1: n .
    c = '#' ifTrue:[ | nextByte |
      nextByte := self _rubyAt1: n + 1  .  "atOrNil: n + 1"
      (nextByte = '$' or:[ nextByte = '@' or:[ nextByte = '{']]) ifFalse:[
         "next char not one of  $  @  {  ,  do not escape " 
        aString add: c .
        chDone  := true .
      ] .
    ] .
    chDone ifNil:[
      av := c _rubyOrd .
      xlated := RubyEscapeTranslationTable at: (av + 1) .
      (xlated == 0) ifTrue: [
	(((av between: 32 and: 126) or: (av between: 160 and: 255)) or: (av = 133)) ifTrue: [
	  aString add: c.
	] ifFalse: [
	  vArr at: 1 put: av .
	  aString addAll: (Module sprintf:'\u%04X' with: vArr)
	] .
      ] ifFalse: [
	aString add: $\ ; add: ( charCls withValue: xlated) .
      ] .
    ] .
  ] .
%

method: String
rubySelectorPrefix
  "return the selector prefix of the receiver.

  ruby selector format is  prefix#N*&
    # is always  $#
    N is one character $0 .. $z
    number of fixed args is N - $0
    * is either $*  or $_
    & is either $&  or $_
  "
  		"ruby_selector_suffix dependent"
  <primitive: 794>   "primitive fails if receiver is a large object"
  | sz |
  (sz := self size) > 1024 ifTrue:[ Error signal:'max Symbol size is 1024' ].
  sz < 4 ifTrue:[ Error signal:'missing ruby selector suffix' ].
  (self _rubyAt1: -4) = '#'"$#" ifFalse:[ Error signal:'invalid ruby selector suffix'].

  self _primitiveFailed: #rubySelectorPrefix args: #() .
%

method: String
rubySelectorPrefixSymbol
  " Return the selector prefix of the receiver as a Symbol

  ruby selector format is  prefix#N*&
    # is always  $#
    N is one character $0 .. $z
    number of fixed args is N - $0
    * is either $*  or $_
    & is either $&  or $_
  "
  		"ruby_selector_suffix dependent"
  <primitive: 812>
  | sz |
  (sz := self size) > 1024 ifTrue:[ Error signal:'max Symbol size is 1024' ].
  sz < 4 ifTrue:[ Error signal:'missing ruby selector suffix' ].
  (self _rubyAt1: -4) = '#' "$#"  ifFalse:[ Error signal:'invalid ruby selector suffix'].
  self _primitiveFailed: #rubySelectorPrefixSymbol args: #() .
%

method:
rubyConcatenate: aString
  <primitive: 819>
  "envId := self __threadSaveCallerEnv ."
  aString _isRubyString ifFalse:[ | str |
    [ 
      str := aString @ruby1:to_str 
    ] onSynchronous: AbstractException do:[ :ex| "swallow ex" ] .
      str _isRubyString ifFalse:[
        ArgumentTypeError signal:'String#+ , cannot convert arg to String with to_str'. 
      ].
    ^ self rubyConcatenate: str 
  ].
  self _primitiveFailed: #rubyConcatenate: args: { aString }
%

method:
_rubyEqual1: aString
  "A Ruby primitive"
  <primitive: 843>
  ^ self @ruby1:__prim_equal_failed: aString
%
method:
_rubyCompare1: aString
  "A Ruby primitive"
  <primitive: 844>
  ^ self @ruby1:__prim_compare_failed: aString
%

method:
_rubyOrd
  ^ (self forRuby at: 1) asInteger
%

method:
_at: anIndex equals: aString

 "A Ruby primitive.
  Returns true if aCharCollection is contained in the receiver, starting at
  anIndex.  Returns false otherwise.  The comparison is case-sensitive.

 Allows anIndex == one past end of receiver and returns false . "
   
 <primitive: 778>
 | sz |
 anIndex _isSmallInteger ifFalse:[ anIndex _validateClass: SmallInteger].
 anIndex == ((sz := self size) + 1) ifTrue:[ ^ false ].
 ((anIndex <= 0) or: [(anIndex > sz)]) ifTrue: [ 
     ^ self _errorIndexOutOfRange: anIndex 
 ].
 self _primitiveFailed: #_at:equals: args: { anIndex . aString }
%

method: 
_asSymbolWithRubySuffix: argInt

"Return a Symbol consisting of self concatenated with specified
  ruby suffix .

 argInt contains bits described by these masks:
     16rFFC - number of fixed args 0..74
     16r2 - append a $*
     16r1 - append a $&
"
  	"send sites are ruby_selector_suffix dependent"
<primitive: 809>   "primitive fails if receiver is large object or DoubleByteSymbol"
  | sz |
  (sz := self size) > 1020 ifTrue:[ Error signal:'max Symbol size is 1024' ].
  (argInt // 4) > 74 ifTrue:[ Error signal:'max of 74 fixed args exceeded' ].

  self _primitiveFailed: #_asSymbolWithRubySuffix: args: { argInt } .
%

!----------------------------------
set class MultiByteString
method: 
_asSymbolWithRubySuffix: argInt
| str |
str := self _reduce .
str charSize == 1 ifFalse:[ Error signal:'MultiByte ruby selectors not supported'].
^ str _asSymbolWithRubySuffix: argInt
%

