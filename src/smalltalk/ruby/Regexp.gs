!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: Regexp.gs 26114 2011-07-08 16:18:18Z stever $
!
!  Smalltalk methods for class Regexp
!=========================================================================

set class  Regexp

removeallmethods
removeallclassmethods

category: 'Ruby private'
classmethod:
_basicNew

  ^ self rubyNewCFinalizer_stBaseClass: Regexp
%
classmethod:
basicNew

 "disallowed"
  self shouldNotImplement: #basicNew
%
classmethod:
new
 "disallowed"
  self shouldNotImplement: #new
%



category: 'Ruby support'
classmethod: 
new: aStringOrRegexp
  "Return a new instance of the receiver, 
  for specified pattern string .

  Regexp variant of argument not implemented yet
  "

^ (self rubyNewCFinalizer_stBaseClass: Regexp)
         _compile: aStringOrRegexp options: 0
%

classmethod:
new: aString options: anInteger
  "Return a new instance of the receiver, 
  for specified pattern string and options.  
  langString must specifiy ASCII ."

  ^ (self rubyNewCFinalizer_stBaseClass: Regexp)
	 _compile: aString options: anInteger
%
classmethod:
new: aString options: anInteger lang: langString

  "Return a new instance of the receiver, 
  for specified pattern string and options.  
  langString must specifiy ASCII ."

  | res status opts unescapedString |

  aString _isRegexp ifTrue:[ ^ aString copy ].
  
  (opts := anInteger) _isSmallInteger ifFalse:[
    ArgumentError signal: 'options should be a SmallInteger'
  ].
  langString ifNotNil:[
    langString _isOneByteString ifFalse:[
      ArgumentError signal: 'lang string should be a String'.
    ].
    opts := self optsFromLang: langString opts: opts .
  ].

  "FIXME: This is a workaround for a bug in the parser. It's compile time, 
   so shouldn't be too bad, but remove this once GitHub issue #142 is fixed
   properly"
  unescapedString := aString copyReplaceAll: '\/' with: '/'.
  res := self rubyNewCFinalizer_stBaseClass: Regexp .
  status := res _compile: unescapedString options: opts .
  status ~~ res ifTrue:[ RegexpError signal: status ].
  ^ res
%

classmethod:
optsFromLang: aString opts: opts
  | ch langbits |
  aString size == 0 ifTrue:[ ^ opts ].
  ch := (aString at: 1) asLowercase  .
  (ch == $n and:[
    (aString equalsNoCase:'n') or:[aString equalsNoCase:'none']]) ifTrue:[
      langbits := KCODE_NONE
  ] ifFalse:[
  (ch == $u and:[
    (aString equalsNoCase:'u') or:[aString equalsNoCase:'utf8']]) ifTrue:[
      langbits := KCODE_UTF8 .
  ] ifFalse:[
  (ch == $e and:[
    (aString equalsNoCase:'e') or:[aString equalsNoCase:'euc']]) ifTrue:[
      langbits := KCODE_EUC .
  ] ifFalse:[
  (ch == $s and:[
    (aString equalsNoCase:'s') or:[aString equalsNoCase:'sjis']]) ifTrue:[
      langbits := KCODE_SJIS .
  ]]]].
  langbits ifNotNil:[
     ^ (opts bitAnd: 16rFFFFFFFF) bitOr:( langbits bitShift: 32"KCODE_shift" ). 
  ].
  ^ opts
%

!------------------ Regexp instance methods 
category: 'Private'

method:
_compile: aString options: anInteger
  ^ self __compile:  (self prepareStringPattern: aString) options: anInteger 
%

method:
__compile: aString options: anInteger
  "Initialize the receiver, for specified pattern string
   and options.  Returns receiver if successful or an error String
   if onig_new() returned non-zero status. 
   
   If anInteger is a SmallInteger , then
     anInteger & 16rFFFFFFFF contains bits per ONIG_OPTION_* in oniguruma.h.
     anInteger & 16rFFFF00000000 contains a KCODE value per one of the
     KCODE* class variables, and is translated to an ONIG_ENCODING* value .
   else anInteger==nil implies  ONIG_OPTION_IGNORECASE,
   else any other value of anInteger implies  ONIG_OPTION_NONE 
   "

  <primitive: 683>

  aString _validateClass:  String  . "Regexp arg not implemented yet"
  ^self _primitiveFailed: #_compile:options: args: { aString . anInteger }
%

method:
unescapeUnicode: aString
  |charSeq result|
  result := aString copyWithRegex: '\\u{[^}]*}' matchesTranslatedUsing: [:useq | |charSeq|
    charSeq := useq copyFrom: 4 to: useq size - 1.
    ((charSeq subStrings: ' ') collect: [:char | self unicodeCharAsString: char]) reduce: [:x :y | x, y]].
  ^ result copyWithRegex: '\\u[0-9|A-F][0-9|A-F][0-9|A-F][0-9|A-F]' matchesTranslatedUsing: [:useq | |charSeq|
    charSeq := useq copyFrom: 3 to: 6.
    self unicodeCharAsString: charSeq]
%

method:
unicodeCharAsString: aCharCodeString
  |unicode|
  unicode := Unicode7 new.
  unicode codePointAt: 1 put: (Integer fromHexString: aCharCodeString).
  "^ (Utf8 fromString: unicode) asString"
  ^ unicode
%

method:
_search: aString from: aStart to: aLimit 
  ^ self __search: aString from: aStart to: aLimit
%

method:
prepareStringPattern: aString
  ^ (self unescapeUnicode: aString asUnicodeString) encodeAsUTF8 asString
%

method:
prepareSearchString: aString
  ^ aString asUnicodeString encodeAsUTF8 asString
%

method:
__search: aString from: aStart to: aLimit 
  "Returns an instance of MatchData or nil. calls onig_search .
   aStart must be a SmallInteger >= 0. 
   if aLimit == nil , limit is aString size
   else if aLimit == true , use  onig_match() searching
      from aStart to  aString.size
   else aLimit must be a SmallInteger >= 0 and <= aString size .
   if aLimit < aStart,  this is a backwards search.
   If aString is nil, returns nil .
   If aString is a String of size 0, returns nil.
  "

  <primitive: 684>
  aString _validateClass:  String .
  "could be failed  recompile after faulting in a persistent Regexp."
  ^ self _primitiveFailed: #_search:from:to: 
         args: { aString . aStart . aLimit }
%

method:
_matchCBytes:aCByteArray from: startOffset limit: limitOffset string: aString
  "Returns an instance of MatchData or nil. 
   calls onig_search to do a match_start search starting at startOffset
   within aCByteArray and searching to the end of aCByteArray .
   startOffset must be a SmallInteger >= 0.
  Returns  nil  if startOffset out of bounds .
  aCByteArray must be of same size as aString, and contain
  same contents as string; primitive fails if sizes do  not match,
  otherwise caller responsible for contents being the same.
  aCByteArray is used by onig_search.  Any resulting MatchData's
  will reference aString . 
  Caller responsible for ensuring that aString is frozen or otherwise
  not modified between calling this method and last reference
  to any MatchData produced by this method."

 <primitive: 824>
  aString _validateClass:  String .
  aCByteArray class == CByteArray 
    ifFalse:[ ArgumentError signal:'not a CByteArray' ].
  aString size == aCByteArray size 
    ifFalse:[ ArgumentError signal:'inconsistent sizes'].
  "could be failed  recompile after faulting in a persistent Regexp."
  ^ self _primitiveFailed: #_matchCBytes:from:limit:string:
         args: { aCByteArray . startOffset . limitOffset . aString }
%


category: 'Ruby support'
method:
rubyPrivateSize
^ 2 "inline  Regexp instSize"
%

method:
source
  "Returns the original pattern string used to create the receiver."
  ^ source
%

method:
options
  "Returns options specified when receiver was created."
  ^ options 
%

method:
kcode
  | kc |
  kc := options bitShift: -32  "- KCODE_shift" .
  kc == 0 ifTrue:[ ^ nil ].
  (kc == KCODE_NONE) ifTrue:[ ^ 'none' copy ].
  (kc == KCODE_UTF8) ifTrue:[ ^ 'utf8' copy ].
  (kc == KCODE_EUC)  ifTrue:[ ^ 'euc' copy ].
  (kc == KCODE_SJIS) ifTrue:[ ^ 'sjis' copy ].
  ^ nil
%
method:
casefold
  "Return true if the IGNORECASE bit is set in the receiver's options"

  ^ (options bitAnd: IGNORECASE) ~~ 0
%

method:
= aRegexp

 "Return true if the receiver and arg have equal source patterns and 
  the same casefold values.
   Note, kcode assumed nil."

  aRegexp _isRegexp ifFalse:[ ^ false ] .
  ^ source = aRegexp source and:[ self casefold == aRegexp casefold ]
%

method
match: aString
  "Returns a MatchData or nil ."

  ^ self _search: aString from: 0 to: nil .
%

method:
to_s
  | optsstr res idx src |  
   "options bit masks MULTILINE = 4; EXTENDED = 2; IGNORECASE = 1 "
  optsstr := #( '(?-mix:'  
               '(?i-mx:'
               '(?x-mi:'
               '(?ix-m:'
               '(?m-ix:'
               '(?mi-x:'
               '(?mx-i:'
               '(?mix:' ) .
  res := (optsstr at: (options bitAnd: 16r7) + 1 ) copy .
  src := source .
  (idx := src indexOf: $/ startingAt: 1 ) == 0 ifTrue:[
    res addAll: src .
  ] ifFalse: [  | prev |
     prev := 1 .
     [
       res add: (src copyFrom: prev to: idx - 1); add:  '\/'.
       prev := idx + 1 .
       idx := src indexOf: $/ startingAt: prev .
       idx ~~ 0
     ] whileTrue .
     res add: (src copyFrom: prev to: src size )  .
  ].
  res add: $) .
  ^ res
%
method:
_regex_to_s
  | optsstr res src |  
   "options bit masks MULTILINE = 4; EXTENDED = 2; IGNORECASE = 1 "
  optsstr := #( '(?-mix:'  
               '(?i-mx:'
               '(?x-mi:'
               '(?ix-m:'
               '(?m-ix:'
               '(?mi-x:'
               '(?mx-i:'
               '(?mix:' ) .
  res := (optsstr at: (options bitAnd: 16r7) + 1 ) copy .
  src := source .
  res addAll: src ;
      add: $) .
  ^ res
%

