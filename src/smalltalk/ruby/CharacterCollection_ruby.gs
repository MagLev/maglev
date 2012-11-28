
!=========================================================================
! Copyright (C) GemStone Systems, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: CharacterCollection_ruby.gs 25969 2012-11-28 23:25:30 NK+KH $
!
!=========================================================================

set class CharacterCollection

!  additional methods  for String to support Ruby
 
category: 'Ruby support'

classmethod:
_installRubyCharacterTranslationTable
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
CharacterCollection _installRubyCharacterTranslationTable .
^ true
%

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -------------------------------- ruby support methods --------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!
!!!!  helper methods used by ruby primitives of the String class


method:
rubyReplaceFrom: start to: end with: aReplacement
  " replace from start to end including end "
  ^ self rubyReplaceFrom: start limit: end + 1 with: aReplacement
%

method:
rubyConcatenate: aString
  self error: 'rubyConcatenate: aString needs to be implemented in CharacterCollection or in subclasses.'.
  self _primitiveFailed: #rubyConcatenate: args: { aString }
%

method:
rubyCompare: aString
  self error: 'rubyCompare: aString needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyAddAll: anArg
  self error: 'rubyAddAll: anArg needs to be implemented in CharacterCollection or in subclasses.'.
  self _primitiveFailed:#_rubyAddAll: args: { anArg }
%

method:
rubyUpperCaseAt: anOffset
  self error: 'rubyUpperCaseAt: anOffset needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyEqual: aString
  self error: 'rubyEqual: aString needs to be implemented in CharacterCollection or in subclasses.'.
  ^ self @ruby1:__prim_equal_failed: aString
%
 
method:
rubyAt: anOffset
  self error: 'rubyAt: anOffset needs to be implemented in CharacterCollection or in subclasses.'.
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
rubyAt: anOffset put: aValue
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
rubyAt: anOffset length: aCount put: aString

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

  ^ self rubyAt: aRange put: aString
%

method:
rubyCapitalize
  self error: 'rubyCapitalize needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyCount: anArray
  self error: 'rubyCount: anArray needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyDelete: a
  self error: 'rubyDelete: a needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyDeleteInPlace: a
  self error: 'rubyDeleteInPlace: a needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyAsLowercase: a
  self error: 'rubyAsLowercase: a needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyDowncaseInPlace: a
  self error: 'rubyDowncaseInPlace: a needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyDumpInto: a
  self error: 'rubyDumpInto: a needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyIsEmpty
  self error: 'rubyIsEmpty needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyEql: other
  self error: 'rubyEql: other needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyHash
  self error: 'rubyHash needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyInsertAll: a at: b
  self error: 'rubyInsertAll: a at: b needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyAsSymbol
  self error: 'rubyAsSymbol needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyInspect
  self error: 'rubyInspect needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyPadded: start to: end withString: aString
  self error: 'rubyPadded: start to: end withString: aString needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyLstrip
  self error: 'rubyLstrip needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyLstripInPlace
  self error: 'rubyLstripInPlace needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyReverse
  self error: 'rubyReverse needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyReverseFrom: anOffset
  self error: 'rubyReverseFrom: anOffset needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyFindLastSubString: subString startingAt: startIndex
  self error: 'rubyFindLastSubString: subString startingAt: startIndex needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyIndexOfLastCharacter: aCharacter startingAt: anOffset
  self error: 'rubyIndexOfLastCharacter: aCharacter startingAt: anOffset needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyIndexOfCharacter: aCharacter startingAt: anOffset
  self error: 'rubyIndexOfCharacter: aCharacter startingAt: anOffset needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyOrd
  self error: 'rubyOrd needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyRstrip
  self error: 'rubyRstrip needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyRstripInPlace
  self error: 'rubyRstripInPlace needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubySize
  self error: 'rubySize needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubySize: anInteger
  self error: 'rubySize: anInteger needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyAt: anIndex equals: aString
  self error: 'rubyAt: anIndex equals: aString needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubySqueeze: a
  self error: 'rubySqueeze: a needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubySqueeze
  self error: 'rubySqueeze needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubySqueezeSelf: a
  self error: 'rubySqueezeSelf: a needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubySqueezeSelf
  self error: 'rubySqueezeSelf needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyStrip
  self error: 'rubyStrip needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyStripInPlace
  self error: 'rubyStripInPlace needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubySucc
  self error: 'rubySucc needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubySwapcaseInPlace
  self error: 'rubySwapcaseInPlace needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyAsFloat
  self error: 'rubyAsFloat needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyTrFrom: a to: b
  self error: 'rubyTrFrom: a to: b needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyTrSqueezeFrom: a to: b
  self error: 'rubyTrSqueezeFrom: a to: b needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyUnpack: aString
  self error: 'rubyUnpack: aString needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyAsUppercase
  self error: 'rubyAsUppercase needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyUpcaseInPlace
  self error: 'rubyUpcaseInPlace needs to be implemented in CharacterCollection or in subclasses.'.
%

method:
rubyReplaceFrom: start to: end with: aReplacement
  self error: 'rubyReplaceFrom: start to: end with: aReplacement needs to be implemented in CharacterCollection or in subclasses.'.
%


