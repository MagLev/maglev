!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: Array_ruby.gs 26189 2011-07-18 17:02:57Z otisa $
!
!  additional methods  for Array to support Ruby
!=========================================================================

set class Array

! adding additional methods , don't 'removeallmethods'

category: 'Ruby support'

classmethod:
_rubyNew: aSize initValue: aVal
| arr |
arr := self rubyBasicNew: aSize .
(aSize ~~ 0 and:[ aVal ~~ nil ]) ifTrue:[
  arr fillFrom: 1 resizeTo: aSize with: aVal .
].
^ arr
%

classmethod:
_rubyWithAll: anArray
  | arr |
  arr := self rubyBasicNew: 0 .
  arr addAll: anArray .
  ^ arr
%

! atOrNil: is in base smalltalk image now

method:
atOrNoArg: idxFlags

"Return the value   self at: anIndex ,
 or return _rubyNoArgNil if anIndex is out of range . "

<primitive: 747>
| maxArgs idx |
idxFlags _isInteger ifTrue:[
  maxArgs := idxFlags bitShift: -8 .
  idx := idxFlags bitAnd: 16rFF .
  ( maxArgs ~~ 0 and: [ self size > (maxArgs - 1) ])  ifTrue:[
    ArgumentError signal: 'too many arguments'
  ].
] ifFalse:[
  self _errorNonIntegerIndex: idxFlags .
].
self _primitiveFailed: #atOrNoArg: args: { idxFlags }
%

method:
atOrMissingArgErr: idxFlags

"Return the value     self at: (idxFlags bitAnd:16rFF)  ,
 or raise an ArgumentError if anIndex is out of range .
 (idxFlags bitShift:-8) is the maxArgs value , non-zero if
 a  too-many-args check is required."

<primitive: 785>
| maxArgs idx |
idxFlags _isInteger ifTrue:[
  maxArgs := idxFlags bitShift: -8 .
  idx := idxFlags bitAnd: 16rFF .
  idx > self size ifTrue:[ ArgumentError signal: 'too few arguments' ].
  ( maxArgs ~~ 0 and: [ self size > (maxArgs - 1) ])  ifTrue:[
    ArgumentError signal: 'too many arguments'
  ].
] ifFalse:[
  self _errorNonIntegerIndex: idxFlags .
].
self _primitiveFailed: #atOrMissingArgErr: args: { idxFlags }
%


method:
_rubyAddLast: anObject

"Appends anObject to the receiver and returns the receiver."

<primitive: 680>
self _primitiveFailed: #_rubyAddLast: args: { anObject }
%

method:
_rubyAt: anOffset
  "a ruby primitive.
   implements Ruby  anArray[anIntOrRange]

   If argument is an SmallInteger (or can be coerced to SmallInteger),
     anOffset is zero based.
     negative offsets go backwards from end, -1 means last element.
     returns nil if anOffset is out of range.
   Else argument must be a Range describing start and end positions.
  "
<primitive: 681>
| rcvrSize aRange start end |
anOffset _isRange ifTrue:[ "a Range"
   "self __threadSaveCallerEnv . "
   aRange := anOffset .
   (start := aRange begin) _isSmallInteger ifFalse:[
     start := [:ofs| ofs @ruby1:to_int ] value: start
           onSynchronous: Exception do:[:ex | ArgumentTypeError signal:'to_int on range start failed'].
   ].
   (end := aRange end) _isSmallInteger ifFalse:[
     end := [:ofs| ofs @ruby1:to_int ] value: end
           onSynchronous: Exception do:[:ex | ArgumentTypeError signal:'to_int on range end failed'].
   ].
   start  _isSmallInteger ifTrue:[
     rcvrSize := self size .
     "If start index equals the array size, and a length or range
      is given, an empty array is returned -- pickaxe"
     start == rcvrSize ifTrue: [  ^ { } ] .
     start < 0 ifTrue:[ start := start + rcvrSize ] .
     end < 0 ifTrue:[ end := end + rcvrSize  ].
     aRange excludeEnd ifTrue:[ end := end - 1 ] .
     (start >= 0 and:[ start < rcvrSize ]) ifTrue:[ | last |
       start > end ifTrue:[ ^ { }  ].
       ^ self copyFrom: start + 1 to: (last := end + 1 min: rcvrSize).
     ].
     ^ nil .
   ].
].
anOffset _isFloat ifTrue:[
  ^ self _rubyAt: anOffset truncated
].
"self __threadSaveCallerEnv ."
start := [:ofs| ofs @ruby1:to_int ] value: anOffset
              onSynchronous: Exception do:[:ex | ArgumentTypeError signal:'to_int failed'].
start _isSmallInteger ifTrue:[
  ^ self _rubyAt: start
] ifFalse:[
  "Should not get here..."
  anOffset _error: #rtErrBadArgKind args:{ SmallInteger . Range }.
].
self _primitiveFailed: #_rubyAt: args: { anOffset }
%

method:
_rubyAt: anOffset length: aCount

 "a ruby primitive.
  Arguments must be SmallIntegers or coercable via ruby to_int .
   Returns an Array containing specified elements of the receiver,
   or returns nil if anOffset is out of range.
   anOffset is zero based.
   negative offsets go backwards from end,  -1 means last element."

  | start end theCount rcvrSize |
  start := (anOffset _isSmallInteger) ifTrue:[ 
    anOffset 
  ] ifFalse:[ 
    "self __threadSaveCallerEnv ."
    [:ofs| ofs @ruby1:to_int ] value: anOffset
                   onSynchronous: Exception do:[:ex | ArgumentTypeError signal:'to_int failed']
  ].

  (aCount _isSmallInteger) ifTrue: [ 
    theCount := aCount 
  ] ifFalse:[ 
    aCount _isFloat ifTrue:[ 
      theCount := aCount truncated 
    ] ifFalse: [
      "self __threadSaveCallerEnv ."
      theCount := [:cnt | cnt @ruby1:to_int ] value: aCount
          onSynchronous: Exception do:[:ex | ArgumentTypeError signal:'to_int failed'].
    ].
    theCount _isSmallInteger ifFalse:[
       OutOfRange signal:'coerced length is not a Fixnum'
    ].
  ].
  theCount < 0 ifTrue: [ ^ nil ] . "If the second arg is negative, return nil, not {}"
  rcvrSize := self size .
  start := start + 1. "convert to 1 based"
  end := start + theCount - 1 .
  start >= 1 ifFalse:[ "going backwards, 0 means last element"
    start := start + rcvrSize .
    end := start + theCount - 1 .
  ].
  (start >= 1 and:[ start <= rcvrSize ]) ifTrue:[  | last |
    ^ self copyFrom: start to: (last := end min: rcvrSize ) .
  ].
  start == (rcvrSize + 1) ifTrue:[ ^ self class new: 0  ].
  ^ nil .
%

method: 
_rubyAt: anOffset put: aValue
  " Ruby  Array  [int]= ,  a ruby primitive.

   If anOffset is an SmallInteger (or coerceable to one),
     Stores aValue at anOffset, growing array as needed.
     anOffset is zero based.
     Negative offsets go backwards from end,  -1 means last element,
     negative offsets that would go beyond start of array generate an error.

   Else argument must be a Range describing start and end positions ,
   which are used to compute start and length, and the store semantics
   are same as    [start,length]=aValue

   Returns aValue
  "
<primitive: 685>
"envId := self __threadSaveCallerEnv ."
anOffset _isRange ifTrue:[  
  |start end rcvrSize count |
   (start := anOffset begin) _isSmallInteger ifFalse:[
     start := [:ofs| ofs @ruby1:to_int ] value: start
           onSynchronous: Exception do:[:ex | ArgumentTypeError signal:'to_int on range start failed'].
   ]. 
   (end := anOffset end) _isSmallInteger ifFalse:[
     end := [:ofs| ofs @ruby1:to_int ] value: end
           onSynchronous: Exception do:[:ex | ArgumentTypeError signal:'to_int on range end failed'].
   ].
   rcvrSize := self size .
   (start > end and:[ end > 0 ] ) ifTrue:[ 
     count := 0  "insert aValue semantics" .
   ] ifFalse:[  
     start < 0 ifTrue:[ 
       start := start + rcvrSize . 
       start < 0 ifTrue:[ ^ OffsetError signal:'In Array#[aRange]= , adjusted start offset < 0'].
     ] .
     end < 0 ifTrue:[ end := end + rcvrSize ] .
     count :=  anOffset excludeEnd ifTrue:[ end - start ] ifFalse:[ end - start + 1 ].
   ].
   ^ self _rubyAt: start length: count put: aValue env: 1"envId"
].
(anOffset _isArray and:[ anOffset size == 2]) ifTrue:[
  ^ self _rubyAt: (anOffset at:1) length: (anOffset at:2) put: aValue env: 1"envId"
].
anOffset _isFloat ifTrue:[
  ^ self _rubyAt: anOffset truncated put: aValue
].
anOffset _isSmallInteger ifTrue:[
  OffsetError new offset: anOffset maximum: self size ; signal 
] ifFalse:[ | ofs |
  ofs := [:arg| arg @ruby1:to_int ] value: anOffset
               onSynchronous: Exception do:[:ex | ArgumentTypeError signal:'to_int failed'].
  ^ self _rubyAt: ofs put: aValue .
].
self _primitiveFailed: #_rubyAt: args: { anOffset . aValue }
%


method:
_rubyToArray: anArg env: envId
  (anArg _isArray) ifTrue: [ ^ anArg ] .
  anArg ifNil: [ ^ { }  ] .
  "envId == 1 ifTrue:["
  (anArg @ruby1:respond_to?: #'to_ary') ifTrue: [ ^ anArg @ruby1:to_ary ] .
  ^ { anArg }
%

method:
_rubyToInt: anArg env: envId
  | ofs |
  "envId == 1 ifTrue:["
  ofs :=  [ anArg @ruby1:to_int 
          ] onSynchronous: Exception do:[:ex | 
            ArgumentTypeError signal:'to_int failed'
          ].
  ofs _isSmallInteger ifFalse:[  OutOfRange signal:'expected a Fixnum'].
  ^ ofs 
%
method:
_rubyAt: anOffset length: aCount put: aValue 
  "a ruby primitive, returns aValue"
^ self _rubyAt: anOffset length: aCount put: aValue env: 1"__callerEnvId"
%

method: 
_rubyAt: anOffset length: aCount put: aValue env: envId
  " Ruby Array  [anOffset, aCount]=aValue

   Replace the slice of self, starting at anOffset and of aCount length,
   with the contents of aValue.  If aValue is nil or the empty array, delete
   aCount elements from receiver starting at anOffset.

   else if aValue is an Array , replace elements anOffset..(anOffset+aCount-1)
   of the receiver with the elements of aValue , growing or shrinking
   receiver as needed ;

   anOffset is zero based.
   Returns the coerced aValue ."

 | sliceStart sliceEnd sliceSize rcvrSize replacement replaceSize newRcvrSize |

  sliceStart := anOffset _isSmallInteger ifTrue:[ anOffset ]
		ifFalse:[ self _rubyToInt: anOffset env: envId ].  "zero based"
  sliceSize := aCount _isSmallInteger ifTrue:[ aCount ]
                ifFalse:[ self _rubyToInt: aCount env: envId ].  "zero based"
  rcvrSize := self size .
  replacement := self _rubyToArray: aValue env: envId.
  replaceSize := replacement size .
  (sliceSize < 0) ifTrue: [ 
     replaceSize == 0 ifTrue:[ ^ aValue ] .
     ^ OffsetError signal:'In Array#[offset,count]= , count < 0'  
  ].
  (sliceStart < 0) ifTrue: [
    sliceStart := sliceStart + rcvrSize .
    (sliceStart < 0) ifTrue: [ ^ OffsetError signal:'In Array#[offset,count]=, offset < 0'].
  ] .
  sliceEnd := sliceStart + sliceSize .  "zero based"
  sliceEnd >= rcvrSize ifTrue:[ sliceSize := rcvrSize - sliceStart ].

  (sliceStart >= rcvrSize) ifTrue: [
    self size: (sliceStart + replaceSize) .
    (replaceSize ~~ 0) ifTrue: [
      self replaceFrom: sliceStart + 1 to: sliceStart + replaceSize  
	   with: replacement startingAt: 1 
     ] .
     ^ aValue
  ] .

  newRcvrSize := rcvrSize.
  replaceSize ~~ sliceSize ifTrue:[
    (replaceSize > sliceSize) ifTrue: [
      newRcvrSize := rcvrSize + replaceSize - sliceSize .
    ] ifFalse: [
      newRcvrSize := rcvrSize - (sliceSize - replaceSize)
    ].
  ].

  (newRcvrSize > rcvrSize) ifTrue: [ self size: newRcvrSize ].
  (sliceEnd < rcvrSize) ifTrue: [ | ofs |
    "Only copy the surviving tail of the original array, if there is one"
      "self copyFrom: (sliceEnd + 1) to: (rcvrSize) into: self 
           startingAt: (sliceStart + replaceSize + 1) ."
      self replaceFrom: (ofs := sliceStart + replaceSize) + 1
           to: ofs + rcvrSize - sliceEnd  with: self startingAt: (sliceEnd + 1) .
  ] .
  (newRcvrSize < rcvrSize) ifTrue: [ self size: newRcvrSize ].
  replaceSize ~~ 0 ifTrue: [
    self replaceFrom: sliceStart + 1  to: sliceStart + replaceSize 
          with: replacement startingAt: 1 .
  ] .
  ^ aValue
%

method:
removeAll
  "remove all elements from receiver."

  self size: 0 .
  ^ self
%

method:
rubyPack: aString
 "A ruby primitive.
  Per PickAxe page 435, table 27-1 .
  TODO:  type in all the pack documentation here.
  "
<primitive: 728>
aString _isOneByteString ifFalse:[
  String _error: #rtErrBadArgKind args:{ String }
].
self _primitiveFailed: #rubyPack: args: { aString }
%

method:
rubyReplace: anArray
  "Replace contents of receiver with contents of anArray"
  | argSize |
  self size: (argSize:= anArray size) .
  argSize ~~ 0 ifTrue:[
    self replaceFrom: 1 to: argSize with: anArray startingAt: 1 .
  ].
  ^ self
%

method:
_rubyAddAll: anArray

"Adds all of the elements of anArray to the receiver and returns
 the receiver. "

anArray _isArray ifTrue:[
  (anArray size ~~ 0) ifTrue:[
    "insert prim handles  self == aCollection ok"
    self insertAll: anArray at: (self size + 1).
  ].
  ^ self
].
anArray ifNotNil:[ ArgumentTypeError signal:'not an Array'].
^ self
%

method:
_rubyDeleteAt: anOffset

"anOffset is zero based.
 Nnegative offsets count backwards from end
 Returns the deleted element, or nil if anOffset is out of range. "
| idx sz elem |
sz := self size .
(idx := anOffset) < 0 ifTrue:[
  idx := sz + anOffset .
  idx < 0 ifTrue:[
    ^ nil .
  ].
].
idx := idx + 1  . "convert to 1 based"
idx > sz ifTrue:[
  ^ nil
].
elem := self at: idx .
self removeFrom: idx to: idx .
^ elem
%
