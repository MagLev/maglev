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
anOffset isInteger ifTrue:[
  ^ self _rubyAt1: anOffset length: 1.
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
^ self @ruby1:__prim_at_failed: anOffset
%

! old version of _rubyAt1:. Returns an integer value of the character
method:
_rubyOrdAt: anInteger
<primitive: 686>
^ self @ruby1:__prim_at_failed: anInteger
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
_rubyAt1: anOffset length: aCount

 "A ruby primitive.
  Returns an instance of receiver's class
   containing specified substring of the receiver,
   or returns nil if anOffset is out of range.
  Negative offsets go backwards from end,  -1 means last element.
  For env 1.
"
<primitive: 687>
^ self @ruby1:__prim_at_length_failed: anOffset _: aCount
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
  <primitive: 690>
  anOffset _isOneByteString ifTrue:[  "anOffset is a String"  | argString ofs argSize |
    argString := anOffset .
    ofs := self _findString: argString startingAt: 1 ignoreCase: false .
    ofs == 0 ifTrue:[
       ^ OffsetError signal:'argument string not found'.
    ].
    ^ self _rubyAt1: ofs - 1 length: argString size put: aValue
  ].
  anOffset _isRegexp ifTrue:[ "anOffset is a Regexp" |aMatchData mOfs mStart mLimit|
    aMatchData := anOffset match: self  .
    aMatchData == nil ifTrue:[
        ^ OffsetError signal:'argument regex not found'.
    ].
    mStart := aMatchData at: 1 . "mStart is zero based "
    mLimit := aMatchData at: 2 .
    ^ self _rubyAt1: mStart length: mLimit - mStart  put: aValue
  ].
  ^ self @ruby1:__prim_at_put_failed: anOffset _: aValue
%

method:
_rubyAt1: offsetArg length: aCount put: aString

  "If offsetArg is an SmallInteger,  anOffset is zero based , and
   replace elements anOffset .. aCount-1 of receiver with contents of aString .

   If offsetArg is a Regexp , replace the portion of the receiver
   specified by  (offsetArg.match(self))[aCount] with aString ,
   where aCount is zero based.

   returns aString."

  | anOffset valSize start end rcvrSize newSize matchSize cnt |
  aCount _isSmallInteger ifFalse:[
    ^ self @ruby1:__prim_at_length_put_failed: offsetArg _: aCount _: aString
  ].
  aString _isOneByteString ifFalse:[
    ^ self @ruby1:__prim_at_length_put_failed: offsetArg _: aCount _: aString
  ].
  anOffset := offsetArg .
  offsetArg _isSmallInteger ifTrue:[
    rcvrSize := self size .
    start := anOffset .  "zero based, first elem to replace"
    start < 0 ifTrue:[ 
      start := start + rcvrSize .
      start < 0 ifTrue:[ ^ OffsetError signal: 'String#[index,count]=str, index too small' ].
    ].
    start > rcvrSize ifTrue:[ ^ OffsetError signal: 'String#[index,count]=str, index too large'].
    aCount < 0 ifTrue:[ OffsetError signal: 'String#[index,count]= , count < 0' ].
    end := start + aCount - 1 . "end is zero based, last elem to replace"
    end >= rcvrSize ifTrue:[ end := rcvrSize - 1 ].
    matchSize := end - start + 1 .
  ] ifFalse:[ "expect a Regexp "  | aMatchData mOfs |
    anOffset _isRegexp ifFalse:[
      ^ self @ruby1:__prim_at_length_put_failed: offsetArg _: aCount _: aString
    ].
    aMatchData := anOffset"aRegexp" match: self  . "anOffset is a Regexp"
    aMatchData == nil ifTrue:[
       ^ OffsetError signal:'argument regex not found'.
    ].
    aCount < 0 ifTrue:[  
      cnt := (aMatchData size // 2) "nMatches" + aCount .
      cnt <= 0 ifTrue:[ ^ OffsetError signal:'String#[regexp,count] , count out of range'].
      mOfs := (cnt * 2) + 1 .
    ] ifFalse:[
      mOfs := (aCount * 2) + 1 . "mOfs is one based"
      mOfs > aMatchData size ifTrue:[ 
         ^ OffsetError signal:'String#[regexp,count] , count out of range'
      ]
    ].
    start := (aMatchData at: mOfs)  . "(zero based matchStart)  "
    end := aMatchData at: mOfs + 1 .     "zero based matchLimit == zero-based end"
    matchSize := end - start .
    rcvrSize := self size .
  ].
  start := start + 1 . "convert to one based"
  end := end + 1 .
  valSize := aString size .
  newSize := rcvrSize + valSize - matchSize .
  newSize < 0 ifTrue: [
    self size: 0 .
    ^ aString .
  ] .
  valSize < matchSize ifTrue:[ "shrinking receiver"
    end < rcvrSize ifTrue: [  
      self replaceFrom:  (cnt := start + valSize)"destIdx" 
           to: cnt + rcvrSize - (end + 1) with: self startingAt: end + 1
    ] .
    self size: newSize .
  ] ifFalse:[ "growing receiver" 
      self size: newSize .
      cnt "numToSave" := rcvrSize - end .
      cnt > 0 ifTrue:[
        self replaceFrom: (cnt := newSize - cnt + 1) 
             to: cnt + rcvrSize - (end + 1) with: self startingAt: end + 1 .
      ].
  ].
  valSize ~~ 0 ifTrue: [
    self replaceFrom: start to: start + valSize - 1 with: aString startingAt: 1 
  ] .
  ^ aString
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
  1 to: self size do: [:n | | c xlated av chDone |
    c := self at: n .
    c == $# ifTrue:[ | nextByte |
      nextByte := self _rubyOrdAt: n  .  "atOrNil: n + 1"
      (nextByte == 36 or:[ nextByte == 64 or:[ nextByte == 123]]) ifFalse:[
         "next char not one of  $  @  {  ,  do not escape " 
        aString add: c .
        chDone  := true .
      ] .
    ] .
    chDone ifNil:[
      av := c asciiValue .
      xlated := RubyEscapeTranslationTable at: (av + 1) .
      (xlated == 0) ifTrue: [
	(av between: 32 and: 126) ifTrue: [
	  aString add: c
	] ifFalse: [
	  vArr at: 1 put: av .
	  aString addAll: (Module sprintf:'\%03o' with: vArr)
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
  (self _rubyOrdAt: -4) == 35"$#" ifFalse:[ Error signal:'invalid ruby selector suffix'].

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
  (self _rubyOrdAt: -4) == 35"$#"  ifFalse:[ Error signal:'invalid ruby selector suffix'].
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

method: 
evaluateInContext: anObject
  ^ self evaluateInContext: anObject symbolList: GsSession currentSession symbolList
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

