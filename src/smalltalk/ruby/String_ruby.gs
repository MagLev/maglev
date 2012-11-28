!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: String_ruby.gs 26189 2011-07-18 17:02:57Z otisa $
!
!=========================================================================

set class String

!
!  additional methods  for String to support Ruby
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -------------------------------- overwritten methods --------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

category: 'Ruby support'

classmethod:
new
  "optimization, reimplement to hide squeak style Object(C)>>new ,
  no need for initialize on byte objects"
  <primitive: 51>
  self _primitiveFailed: #new .
%

method:
executeOnServer
  ^ System performOnServer: self
%

method:
isOctetString
  ^ true
%

method:
asOctetString
  ^ self
%

set class MultiByteString
method: 
_rubyPrim_asSymbolWithRubySuffix: argInt
| str |
str := self _reduce .
str charSize == 1 ifFalse:[ Error signal:'MultiByte ruby selectors not supported'].
^ str _rubyPrim_asSymbolWithRubySuffix: argInt
%
set class String

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -------------------------------- overwritten methods --------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -------------------------------- encoding methods --------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

category: 'Ruby support-encoding'


classmethod:
fromForRuby: anObjectForRuby
  "see the instance side >>forRuby. creates a String object from aString forRuby"
  ^ anObjectForRuby encodeAsUTF8 asByteArray asString
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
fromRubyDataDoAndChange: aBlock
  " this is where encoding and decoding will take place "
  " TODO: check if return value is self? "
  ^ (aBlock value: self) "toRubyDataWithEncoding: self encoding"
%

method:
fromRubyDataDo: aBlock
  ^ (aBlock value: self) "toRubyDataWithEncoding: self encoding"
%

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -------------------------------- ruby class primitives --------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

category: 'Ruby support-primitives'

!            class_primitive_nobridge '__withAll', '_rubyPrim_WithAll:'
classmethod:
_rubyPrim_WithAll: aSequenceableCollection
  ^ self withAll: aSequenceableCollection
%

!            class_primitive_nobridge '__alloc', '_rubyPrim_BasicNew'
classmethod:
_rubyPrim_BasicNew
  ^ self _basicNew

!            class_primitive_nobridge '__new', '_rubyPrim_New:'
classmethod: 
_rubyPrim_New: anObject
  ^ self new: anObject
%


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! --------------------------------  encoding methods end --------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -------------------------------- ruby encoding primitives --------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



category: 'Ruby support-primitives-encoding'

method:
_rubyPrim_CopyFrom: start to: end
  ^ self fromRubyDataDo: [ :o | o rubyCopyFrom: start to: end].
%
	
method:
_rubyPrim_FindString: aString startingAt: anOffset
  ^ self fromRubyDataDo: [ :o | o rubyFindString: aString startingAt: anOffset]. 
%

method:
_rubyPrim_Md5sumDigest
  ^ self fromRubyDataDo: [ :o | o rubyMd5sumDigest].
%

method:
_rubyPrim_RemoveFrom: start to: end
  ^ self fromRubyDataDoAndChange: [ :o | o rubyRemoveFrom: start to: end].
%

method:
_rubyPrim_BasicDup
  ^ self fromRubyDataDo: [ :o | o rubyBasicDup].
%

method:
_rubyPrim_Concatenate: aString
  ^ self fromRubyDataDoAndChange: [ :o | o rubyConcatenate: aString].
%


method:
_rubyPrim_Compare1: aString
  ^ self fromRubyDataDo: [ :o | o rubyCompare: aString].
%

method:
_rubyPrim_AddAll: anArg
  ^ self fromRubyDataDoAndChange: [:o | o rubyAddAll: anArg].
%

method:
_rubyPrim_UpperCaseAt: anOffset
  ^ self fromRubyDataDoAndChange: [:o | o rubyUpperCaseAt: anOffset].
%

method:
_rubyPrim_Equal1: aString
  ^ self fromRubyDataDo: [:o | o rubyEqual: aString].
%

method:
_rubyPrim_At1: anOffset
  ^ self fromRubyDataDo: [:o | o rubyAt: anOffset].
%

method:
_rubyPrim_At1: anOffset length: aCount
  ^ self fromRubyDataDo: [:o | o rubyAt: anOffset length: aCount].
%


method:
_rubyPrim_At1: anOffset put: aValue
  ^ self fromRubyDataDoAndChange: [:o | o rubyAt: anOffset put: aValue].
%

method:
_rubyPrim_At1: anOffset length: aCount put: aString
  ^ self fromRubyDataDoAndChange: [:o | o rubyAt: anOffset length: aCount put: aValue].
%

method:
_rubyPrim_Capitalize
  ^ self fromRubyDataDoAndChange: [:o | o rubyCapitalize].
%

method:
_rubyPrim_Count: anArray
  ^ self fromRubyDataDoAndChange: [:o | o rubyCount: anArray].
%

method:
_rubyPrim_Delete: a
  ^ self fromRubyDataDoAndChange: [:o | o rubyDelete: a].
%

method:
_rubyPrim_DeleteInPlace: a
  ^ self fromRubyDataDoAndChange: [:o | o rubyDeleteInPlace: a].
%

method:
_rubyPrim_AsLowercase: a
  ^ self fromRubyDataDoAndChange: [:o | o rubyAsLowercase: a].
%

method:
_rubyPrim_DowncaseInPlace: a
  ^ self fromRubyDataDoAndChange: [:o | o rubyDowncaseInPlace: a].
%

method:
_rubyPrim_DumpInto: a
  ^ self fromRubyDataDoAndChange: [:o | o rubyDumpInto: a].
%

method:
_rubyPrim_IsEmpty
  ^ self fromRubyDataDo: [:o | o rubyIsEmpty].
%

method:
_rubyPrim_Eql: other
  " self = other "
  ^ self fromRubyDataDo: [:o | o rubyEql: other].
%

method:
_rubyPrim_Hash
  ^ self fromRubyDataDo: [:o | o rubyHash].
%

method:
_rubyPrim_InsertAll: a at: b
  ^ self fromRubyDataDoAndChange: [:o | o rubyInsertAll: a at: b].
%

method:
_rubyPrim_AsSymbol
  ^ self fromRubyDataDo: [:o | o rubyAsSymbol].
%

method:
_rubyPrim_Inspect
  ^ self fromRubyDataDo: [:o | o rubyInspect].
%

method:
_rubyPrim_Padded: start to: end withString: aString
  ^ self fromRubyDataDoAndChange: [:o | o rubyPadded: start to: end withString: aString].
%

method:
_rubyPrim_Lstrip
  ^ self fromRubyDataDo: [:o | o rubyLstrip].
%

method:
_rubyPrim_LstripInPlace: a
  ^ self fromRubyDataDoAndChange: [:o | o rubyLstripInPlace].
%

method:
_rubyPrim_reverse
  ^ self fromRubyDataDo: [:o | o rubyReverse].
%

method:
_rubyPrim_reverseFrom: anOffset
  ^ self fromRubyDataDoAndChange: [:o | o rubyReverseFrom: anOffset].
%

method:
_rubyPrim_FindLastSubString: subString startingAt: startIndex
  ^ self fromRubyDataDo: [:o | o rubyFindLastSubString: subString startingAt: startIndex].
%

method:
_rubyPrim_Ord
  ^ self fromRubyDataDo: [:o | o rubyOrd].
%

method:
_rubyPrim_Rstrip
  ^ self fromRubyDataDo: [:o | o rubyRstrip].
%

method:
_rubyPrim_RstripInPlace
  ^ self fromRubyDataDoAndChange: [:o | o rubyRstripInPlace].
%

method:
_rubyPrim_Size
  ^ self fromRubyDataDo: [:o | o rubySize].
%

method:
_rubyPrim_Size: anInteger
  ^ self fromRubyDataDoAndChange: [:o | o rubySize: anInteger].
%

method:
_rubyPrim_At: anIndex equals: aString

 "A Ruby primitive.
  Returns true if aCharCollection is contained in the receiver, starting at
  anIndex.  Returns false otherwise.  The comparison is case-sensitive.

  Allows anIndex == one past end of receiver and returns false . "
  ^ self fromRubyDataDo: [:o | o rubyAt: anIndex equals: aString].
%


method:
_rubyPrim_Squeeze: a
  ^ self fromRubyDataDoAndChange: [:o | o rubySqueeze: a].
%

method:
_rubyPrim_Squeeze
  ^ self fromRubyDataDoAndChange: [:o | o rubySqueeze].
%

method:
_rubyPrim_SqueezeSelf: a
  ^ self fromRubyDataDoAndChange: [:o | o rubySqueezeSelf: a].
%

method:
_rubyPrim_SqueezeSelf
  ^ self fromRubyDataDoAndChange: [:o | o rubySqueezeSelf].
%

method:
_rubyPrim_Strip
  ^ self fromRubyDataDo: [:o | o rubyStrip].
%

method:
_rubyPrim_StripInPlace
  ^ self fromRubyDataDo: [:o | o rubyStripInPlace].
%

method:
_rubyPrim_Succ
  ^ self fromRubyDataDo: [:o | o rubySucc].
%

method:
_rubyPrim_SwapcaseInPlace
  ^ self fromRubyDataDoAndChange: [:o | o rubySwapcaseInPlace].
%

method:
_rubyPrim_AsFloat
  ^ self fromRubyDataDo: [:o | o rubyAsFloat].
%

method:
_rubyPrim_TrFrom: a to: b
  ^ self fromRubyDataDoAndChange: [:o | o rubyTrFrom: a to: b].
%

method:
_rubyPrim_TrSqueezeFrom: a to: b
  ^ self fromRubyDataDoAndChange: [:o | o rubyTrSqueezeFrom: a to: b].
%

method:
_rubyPrim_Unpack: aString
  ^ self fromRubyDataDo: [:o | o rubyUnpack: aString].
%

method:
_rubyPrim_AsUppercase
  ^ self fromRubyDataDo: [:o | o rubyAsUppercase].
%

method:
_rubyPrim_UpcaseInPlace: a
  ^ self fromRubyDataDoAndChange: [:o | o rubyUpcaseInPlace].
%

method:
_rubyPrim_ReplaceFrom: start to: end with: aReplacement
  ^ self fromRubyDataDoAndChange: [:o | o rubyReplaceFrom: start to: end with: aReplacement].
%

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -------------------------------- ruby encoding primitives end --------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -------------------------------- ruby non-encoding primitives --------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

category: 'Ruby support-primitives-no-encoding'

method:
_rubyPrim_ByteAt: anOffset
  " This is encoding independent. 
    A String can be asked to return its byte represenation. "
  ^ (self at: anOffset) asInteger
%

method:
_rubyPrim_ByteAt: anOffset length: aCount
  " This is encoding independent. 
    A String can be asked to return its byte represenation. "

  " A ruby primitive.
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
_rubyPrim_Replace: aString

  "Replace contents of receiver with contents of argument.
   Returns the receiver."

  ^ self rubyReplace: aString
%


method:
_rubyPrim_IndexOfLastByte: a startingAt: b
  ^ self _primitiveFailed: #_rubyPrim_indexOfLastByte:startingAt: { a . b }
%

method:
_rubyPrim_IndexOfByte: a startingAt: b
  ^ self _primitiveFailed: #_rubyPrim_indexOfByte:startingAt: { a . b }
%

method:
_rubyPrim_ByteSize
  ^ self size
%

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -------------------------------- ruby non-encoding primitives end --------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -------------------------------- ruby support methods --------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!
!!!!  helper methods used by ruby primitives

category: 'Ruby support'

method:
rubyPrim_ReplaceFrom: start limit: end with: aReplacement
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
rubyCompare: aString
  "A Ruby primitive"
  <primitive: 844>
  ^ self @ruby1:__prim_compare_failed: aString
%

method:
rubyAddAll: anArg

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

method:
rubyEqual: aString
  "A Ruby primitive"
  <primitive: 843>
  ^ self @ruby1:__prim_equal_failed: aString
%

method:
rubyAt: anOffset
  "edits to _rubyAt1:  must be replicated to rubyAt2: for env 2"
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
rubyAt: anOffset length: aCount

  " A ruby primitive.
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
rubyInspect
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
rubyReplace: aString

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
rubyFindLastSubString: subString startingAt: startIndex
  "adapt myself to subString and call the findLastSubString:startingAt: method"
  ^ (self forRubyAdaptedTo: subString) findLastSubString: subString forRuby startingAt: startIndex
%

method:
rubyOrd
  ^ (self forRuby at: 1) asInteger
%

method:
rubyRstripInPlace
  ^ self _rubyRstripInPlace
%

method:
rubyAt: anIndex equals: aString

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
rubyAsFloat
  ^ self asFloat
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
rubyAsUppercase
  ^ self asUppercase
%

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -------------------------------- ruby support methods end --------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -------------------------------- unused methods --------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

category: 'Ruby support-unused methods'

method:
_rubyPrim_At: anOffset length: aCount

 "A ruby primitive.
  Returns an instance of receiver's class
   containing specified substring of the receiver,
   or returns nil if anOffset is out of range.
  Negative offsets go backwards from end,  -1 means last element.
  Used by Smalltalk code in MatchData .
  "
  'seems to be an unused method see /src/smalltalk/ruby/String_ruby.gs' halt.
  "<primitive: 687>"
  ^ self _primitiveFailed: #_rubyAt:length: args: { anOffset . aCount }
%

method:
_rubyPrim_SelectorPrefix
  "return the selector prefix of the receiver.

  ruby selector format is  prefix#N*&
    # is always  $#
    N is one character $0 .. $z
    number of fixed args is N - $0
    * is either $*  or $_
    & is either $&  or $_
  "
  		"ruby_selector_suffix dependent"
  'seems to be an unused method see /src/smalltalk/ruby/String_ruby.gs' halt.
  <primitive: 794>   "primitive fails if receiver is a large object"
  | sz |
  (sz := self size) > 1024 ifTrue:[ Error signal:'max Symbol size is 1024' ].
  sz < 4 ifTrue:[ Error signal:'missing ruby selector suffix' ].
  (self _rubyAt1: -4) = '#'"$#" ifFalse:[ Error signal:'invalid ruby selector suffix'].

  self _primitiveFailed: #_rubyPrim_SelectorPrefix args: #() .
%




method:
_rubyPrim_SelectorPrefixSymbol
  " Return the selector prefix of the receiver as a Symbol

  ruby selector format is  prefix#N*&
    # is always  $#
    N is one character $0 .. $z
    number of fixed args is N - $0
    * is either $*  or $_
    & is either $&  or $_
  "
  		"ruby_selector_suffix dependent"
  'seems to be an unused method see /src/smalltalk/ruby/String_ruby.gs' halt.
  <primitive: 812>
  | sz |
  (sz := self size) > 1024 ifTrue:[ Error signal:'max Symbol size is 1024' ].
  sz < 4 ifTrue:[ Error signal:'missing ruby selector suffix' ].
  (self _rubyAt1: -4) = '#' "$#"  ifFalse:[ Error signal:'invalid ruby selector suffix'].
  self _primitiveFailed: #_rubyPrim_SelectorPrefixSymbol args: #() .
%

method: 
_rubyPrim_asSymbolWithRubySuffix: argInt
  "Return a Symbol consisting of self concatenated with specified
    ruby suffix .

   argInt contains bits described by these masks:
       16rFFC - number of fixed args 0..74
       16r2 - append a $*
       16r1 - append a $&
  "
  	"send sites are ruby_selector_suffix dependent"
  'seems to be an unused method see /src/smalltalk/ruby/String_ruby.gs' halt.
  <primitive: 809>   "primitive fails if receiver is large object or DoubleByteSymbol"
  | sz |
  (sz := self size) > 1020 ifTrue:[ Error signal:'max Symbol size is 1024' ].
  (argInt // 4) > 74 ifTrue:[ Error signal:'max of 74 fixed args exceeded' ].

  self _primitiveFailed: #__rubyPrim_asSymbolWithRubySuffix: args: { argInt } .
%

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -------------------------------- unused methods end --------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!











