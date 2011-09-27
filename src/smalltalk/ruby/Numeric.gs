!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: Numeric.gs 26189 2011-07-18 17:02:57Z otisa $
!
!  This file contains additions to all of the Numeric classes 
!  of additional Smalltalk methods to support the Ruby numeric classes.
!
!  This file is compiled in environment 0, and methods are installed into
!  environment 1 by  Ruby code in  prims.rb .
!=========================================================================

category: 'Ruby numeric'

! _rubyAdd:, _rubySubtract: , _rubyMultiply:  not needed in SmallInteger
!  special sends will fall back to Integer .

method: SmallInteger
_rubyDivide: aNumber

"A ruby primitive. Divides the receiver by aNumber. "

<primitive: 12>
(aNumber == 0) ifTrue: [^ self _errorDivideByZero].
"self __threadSaveCallerEnv . "
^ super _st_rubyDivide:  aNumber
%

method: SmallInteger
_bitShiftRight: aNumber

"Returns the receiver shifted right by aNumber bits"

^ self bitShift: 0 - aNumber .
%

method: SmallInteger
_ruby_id2name

| o |
self <= 0 ifTrue:[ ^ nil ].
o := Object _objectForOop: self .
o _isSymbol ifTrue:[ 
  ^ String withAll: o . " note DoubleByteSymbol not supported"
].
^ nil
%

method: SmallInteger
_rubySize

"Returns number of bytes in machine representation of the receiver"
^ 8
%

method: SmallInteger
_ruby_eqlQ: aNumber

"Return true if aNumber is a SmallInteger with the same value as self."
aNumber _isSmallInteger ifTrue:[
   ^ self = aNumber
].
^ false
%

method: SmallInteger
_rubyLt: aNumber

"A ruby primitive.
 Returns true if the receiver is less than aNumber;
 returns false otherwise. "

<primitive: 3>
"self __threadSaveCallerEnv ."
^ super _st_rubyLt: aNumber
%

method: SmallInteger
_rubyGt: aNumber

"A ruby primitive.
 Returns true if the receiver is greater than aNumber;
 returns false otherwise."

<primitive: 4>
"self __threadSaveCallerEnv ."
^ super _st_rubyGt: aNumber
%

method: SmallInteger
_rubyLteq: aNumber

"A ruby primitive.
 Returns true if the receiver is less than or equal to
 aNumber; returns false otherwise."

<primitive: 5>
"self __threadSaveCallerEnv ."
^ super _st_rubyLteq: aNumber
%

method: SmallInteger
_rubyGteq: aNumber

"A ruby primitive.
 Returns true if the receiver is greater than or equal to
 aNumber; returns false otherwise."

<primitive: 6>
"self __threadSaveCallerEnv ."
^ super _st_rubyGteq: aNumber
%

method: SmallInteger
_rubyEqual: aNumber

"A ruby primitive.
 Returns true if the receiver is equal to
 aNumber; returns false otherwise."

<primitive: 7>
"self __threadSaveCallerEnv ."
^ super _st_rubyEqual: aNumber
%

method: SmallInteger
_rubyModulo: aNumber

"a ruby primitive. Returns the modulus defined in terms of // "
<primitive: 11>
"self __threadSaveCallerEnv ."
^ super _st_rubyModulo: aNumber
%

method: SmallInteger
_rubyBitAnd: aNumber

"A ruby primitive.
 Returns an Integer whose bits are the logical and of the receiver's bits and
 the bits of aNumber."

<primitive: 14>
"self __threadSaveCallerEnv ."
^ super _st_rubyBitAnd: aNumber
%

method: SmallInteger
_rubyBitOr: aNumber

"a ruby primitive.
 Returns an Integer whose bits are the logical or of the receiver's bits and
 the bits of aNumber."

<primitive: 15>
"self __threadSaveCallerEnv ."
^ super _st_rubyBitOr: aNumber
%

method: SmallInteger
_rubyBitXor: aNumber

"a ruby primitive..
 Returns an Integer whose bits are the logical xor of the receiver's bits and
 the bits of aNumber."

<primitive: 16>
"self __threadSaveCallerEnv ."
^ super _st_rubyBitXor: aNumber
%

method: SmallInteger
_rubyShiftLeft: aNumber

" a ruby primitive"
<primitive: 17>
"self __threadSaveCallerEnv ."
^ super _st_rubyShiftLeft: aNumber
%

method: SmallInteger
_rubyRaisedTo: aNumber

"A ruby primitive.
 Returns the receiver raised to the power of the argument."

<primitive: 662>
"primitive handles SmallInteger and Float args. "

"self __threadSaveCallerEnv ."
^ super _st_rubyRaisedTo: aNumber
%

method: SmallInteger
_rubyToSym

"Return the Symbol whose objectId is self, else return nil.
 DoubleByteSymbols  are not returned."
| obj |
obj := Object _objectForOop: self .
(obj _isSymbol and:[ obj _isOneByteString]) ifTrue:[ ^ obj ].
^ nil
%

!----------------------------------------------
classmethod: Integer
fromString: aString radix: aSmallInt
  "result is 0 if aString contains all spaces, or
   first non-space character of aString is not a
   a legal numeric digit for specified radix."
<primitive: 893>
aString _validateClasses: { String . MultiByteString } .
aSmallInt _validateClass: SmallInteger .
(aSmallInt < 0 or:[ aSmallInt > 36]) ifTrue:[
  OutOfRange signal:'radix out of range'
].
self _primitiveFailed: #fromString:radix: args: { aString . aSmallInt }
%

method: Integer
_rubyAdd: anInteger

"Returns the sum of the receiver and anInteger."

<primitive: 258>

^ self _rubyRetry: #'+#1__' coercing: anInteger env: 1"__callerEnvId"
%

method: Integer
_rubySubtract: anInteger

"Returns the difference between the receiver and anInteger."

<primitive: 259>
^ self _rubyRetry: #'-#1__' coercing: anInteger env: 1"__callerEnvId"
%

method: Integer
_rubyMultiply: anInteger

"Returns the product of the receiver and anInteger."

<primitive: 260>

^ self _rubyRetry: #'*#1__' coercing: anInteger env: 1"__callerEnvId"
%

method: Integer
_rubyDivide: aNumber

"a ruby primitive.
 Returns the result of dividing the receiver by anInteger."

<primitive: 263>
(aNumber == 0)
  ifTrue: [^ self _errorDivideByZero]
  ifFalse: [^ self _rubyRetry: #'/#1__' coercing: aNumber env: 1"__callerEnvId" ]
%
method: Integer
_st_rubyDivide: aNumber

"invoked from smalltalk.
 Returns the result of dividing the receiver by anInteger."

<primitive: 263>
(aNumber == 0)
  ifTrue: [^ self _errorDivideByZero]
  ifFalse: [^ self _rubyRetry: #'/#1__' coercing: aNumber env: 1"__threadRubyEnvId" ]
%

method: Integer
_rubyModulo: aNumber

"a ruby primitive.
 Returns the modulus defined in terms of // ."
<primitive: 264>
(aNumber == 0) ifTrue:[ ^ self _errorDivideByZero].
^ self _rubyRetry: #'%#1__' coercing: aNumber env: 1"__callerEnvId"
%
method: Integer
_st_rubyModulo: aNumber

"called from smalltalk.
 Returns the modulus defined in terms of // ."
<primitive: 264>
(aNumber == 0) ifTrue:[ ^ self _errorDivideByZero].
^ self _rubyRetry: #'%#1__' coercing: aNumber env: 1"__threadRubyEnvId"
%


method: Integer
_rubyRaisedTo: aNumber
  "a ruby primitive"
(aNumber _isInteger) ifFalse:[
  aNumber _isNumber ifFalse:[ ArgumentTypeError signal:'argument is not a Numeric'].
  ^ self @ruby1:__fraised_to: aNumber 
].
aNumber < 0 ifTrue:[
  ^ 1.0 / (self _raisedToPositiveInteger: (0 - aNumber))
].
^ [
   self _raisedToPositiveInteger: aNumber
  ] onSynchronous: FloatingPointError do:[:ex |
    ex number == 2503 ifTrue:[  "ex is LargeInteger overflow error"
       ex return:( self asFloat raisedToInteger: aNumber)
    ] .
    ex outer
  ]
%

method: Integer
_st_rubyRaisedTo: aNumber
  "called from smalltalk"
(aNumber _isInteger) ifFalse:[
  aNumber _isNumber ifFalse:[ ArgumentTypeError signal:'argument is not a Numeric'].
  ^ self @ruby1:__fraised_to: aNumber
].
aNumber < 0 ifTrue:[
  ^ 1.0 / (self _raisedToPositiveInteger: (0 - aNumber))
].
^ [
   self _raisedToPositiveInteger: aNumber
  ] onSynchronous: FloatingPointError do:[:ex |
    ex number == 2503 ifTrue:[  "ex is LargeInteger overflow error"
       ex return:( self asFloat raisedToInteger: aNumber)
    ] .
    ex outer
  ]
%

method: Integer
_bitShiftRight: aNumber

"Returns the receiver shifted right by aNumber bits"

^ self bitShift: 0 - aNumber .
%

method: Integer
_ruby_eqlQ: aNumber

"Return true if aNumber is a Bignum with the same value as self."

aNumber _isInteger ifFalse:[ ^ false ].
aNumber _isSmallInteger == self _isSmallInteger ifFalse:[ ^ false ].
^ self = aNumber
%


method: Integer
_rubyLt: aNumber

"A ruby primitive.
 Returns true if the receiver is less than aNumber; returns false otherwise."

<primitive: 20>
aNumber _isFloat ifTrue:[ ^ self asFloat < aNumber ].
^ self _rubyRetry: #'<#1__' coercing: aNumber env: 1"__callerEnvId"
% 
method: Integer
_st_rubyLt: aNumber

"called from smalltalk.
 Returns true if the receiver is less than aNumber; returns false otherwise."

<primitive: 20>
aNumber _isFloat ifTrue:[ ^ self asFloat < aNumber ].
^ self _rubyRetry: #'<#1__' coercing: aNumber env: 1"__threadRubyEnvId"
% 

method: Integer
_rubyLteq: aNumber
 
"A ruby primitive.
 Returns true if the receiver is less than or equal to aNumber; returns false
 otherwise."

<primitive: 21>
aNumber _isFloat ifTrue:[ ^ self asFloat <= aNumber ].
^ self _rubyRetry: #'<=#1__' coercing: aNumber env: 1"__callerEnvId"
%
method: Integer
_st_rubyLteq: aNumber
 
"called from smalltalk.
 Returns true if the receiver is less than or equal to aNumber; returns false
 otherwise."

<primitive: 21>
aNumber _isFloat ifTrue:[ ^ self asFloat <= aNumber ].
^ self _rubyRetry: #'<=#1__' coercing: aNumber env: 1"__threadRubyEnvId"
%

method: Integer
_rubyGt: aNumber

"A ruby primitive.
 Returns true if the receiver is greater than aNumber; returns false
 otherwise."

<primitive: 22>
aNumber _isFloat ifTrue:[ ^ self asFloat > aNumber ].
^ self _rubyRetry: #'>#1__' coercing: aNumber env: 1"__callerEnvId"
%
method: Integer
_st_rubyGt: aNumber

"called from smalltalk.
 Returns true if the receiver is greater than aNumber; returns false
 otherwise."

<primitive: 22>
aNumber _isFloat ifTrue:[ ^ self asFloat > aNumber ].
^ self _rubyRetry: #'>#1__' coercing: aNumber env: 1"__threadRubyEnvId"
%

method: Integer
_rubyGteq: aNumber

"A ruby primitive.
 Returns true if the receive is greater than aNumber; returns false
 otherwise."

<primitive: 23>
aNumber _isFloat ifTrue:[ ^ self asFloat >= aNumber ].
^ self _rubyRetry: #'>=#1__' coercing: aNumber env: 1"__callerEnvId"
%
method: Integer
_st_rubyGteq: aNumber

"called from smalltalk.
 Returns true if the receive is greater than aNumber; returns false
 otherwise."

<primitive: 23>
aNumber _isFloat ifTrue:[ ^ self asFloat >= aNumber ].
^ self _rubyRetry: #'>=#1__' coercing: aNumber env: 1"__threadRubyEnvId"
%

method: Integer
_rubyEqual: aNumber

"A ruby primitive.
 Returns true if the receiver is equal to aNumber; returns false otherwise."

<primitive: 24>
aNumber _isFloat ifTrue:[ ^ self asFloat = aNumber ].
"envId := self __callerEnvId ."
^ [ | arr |
    arr := aNumber @ruby1:coerce: self. "arr is { coercedSelf . coercedNum }"
    (arr at: 1) @ruby1:==: (arr at: 2)
  ] onSynchronous: Exception do:[:ex|
    aNumber @ruby1:==: self
  ]
%

method: Integer
_st_rubyEqual: aNumber

"A ruby primitive.
 Returns true if the receiver is equal to aNumber; returns false otherwise."

<primitive: 24>
aNumber _isFloat ifTrue:[ ^ self asFloat = aNumber ].
"envId := self __threadRubyEnvId .  "
^ [ | arr |
    arr := aNumber @ruby1:coerce: self. "arr is { coercedSelf . coercedNum }"
    (arr at: 1) @ruby1:==: (arr at: 2)
  ] onSynchronous: Exception do:[:ex|
    aNumber @ruby1:==: self 
  ]
%

method: Integer
_rubyBitAnd: anInteger

"a ruby primitive.
 Returns an Integer whose bits are the logical and of the receiver's bits and
 the bits of anInteger."

<primitive: 740>
^ self @ruby1:__bit_and: anInteger
%
method: Integer
_st_rubyBitAnd: anInteger

"called from smalltalk.
 Returns an Integer whose bits are the logical and of the receiver's bits and
 the bits of anInteger."

<primitive: 740>
^ self @ruby1:__bit_and: anInteger
%

method: Integer
_rubyBitOr: anInteger

"A ruby primitive.
 Returns an Integer whose bits are the logical or of the receiver's bits and
 the bits of anInteger."

<primitive: 741>
^ self @ruby1:__bit_or: anInteger
%
method: Integer
_st_rubyBitOr: anInteger

"called from smalltalk.
 Returns an Integer whose bits are the logical or of the receiver's bits and
 the bits of anInteger."

<primitive: 741>
^ self @ruby1:__bit_or: anInteger
%

method: Integer
_rubyBitXor: anInteger

"A ruby primitive.
 Returns an Integer whose bits are the logical xor of the receiver's bits and
 the bits of anInteger."

<primitive: 743>
^ self @ruby1:__bit_xor: anInteger
%
method: Integer
_st_rubyBitXor: anInteger

"called from smalltalk.
 Returns an Integer whose bits are the logical xor of the receiver's bits and
 the bits of anInteger."

<primitive: 743>
^ self @ruby1:__bit_xor: anInteger
%

method: Integer
_rubyShiftLeft: anInteger

"A ruby primitive.
 If anInteger is positive, the shift is to the left"

<primitive: 742>
^ self @ruby1:__shift_left: anInteger
%
method: Integer
_st_rubyShiftLeft: anInteger

"called from smalltalk.
 If anInteger is positive, the shift is to the left"

<primitive: 742>
^ self @ruby1:__shift_left: anInteger
%

! creation of instances of Range
method: Integer
_rubyTo: anInteger

^ Range from: self to: anInteger 
%
method: Integer
_rubyTo_: anInteger

^ Range from: self limit: anInteger 
%

category: 'Ruby numeric'
method: Integer 
_asHexDigest

| digits idx num res |
digits := String new .
self <= 0 ifTrue:[ OutOfRange signal:'_asHexDigest requires positive number'].
num := self .
idx := 0 .
[num = 0] whileFalse: [
  idx := idx + 1 .
  digits codePointAt: idx put: (num bitAnd: 16rFF) .
  num := num bitShift: -8 . 
].
"the bytes are now in reverse order and must be swapped around"
res := String new: idx .
1 to: idx do:[:n |
  res codePointAt: n put: (digits codePointAt: idx - n + 1 )
].
^ res
%



!----------------------------------------------
method: Float
_rubyAdd: aNumber

"Returns the sum of the receiver and aNumber."

<primitive: 106>
^ self _rubyRetry: #'+#1__' coercing: aNumber env: 1"__callerEnvId"
%
method: Float
_rubySubtract: aNumber

"Returns the difference between the receiver and aNumber."

<primitive: 107>
^ self _rubyRetry: #'-#1__' coercing: aNumber env: 1"__callerEnvId"
%
method: Float
_rubyMultiply: aNumber

"Multiply the receiver by aNumber and returns the result."

<primitive: 102>
^ self _rubyRetry: #'*#1__' coercing: aNumber env: 1"__callerEnvId"
%

method: Float
_rubyDivide: aNumber

"Divide the receiver by aNumber and returns the result."

<primitive: 108>
^ self _rubyRetry: #'/#1__' coercing: aNumber env: 1"__callerEnvId"
%

method: Float
_rubyModulo: aNumber

"Divide the receiver by aNumber and returns the remainder.  
 Returns PlusQuietNaN if aNumber == 0.0 "
<primitive: 800>
^ self _rubyRetry: #'%#1__' coercing: aNumber env: 1"__callerEnvId"
%

method: Float
_ruby_eqlQ: aNumber

"If aNumber is an instance of Float or SmallDouble, return
 true if receiver and aNumber have the same numeric value,
 else return false."

aNumber _isFloat ifTrue:[
  ^ self = aNumber .
].
^ false
%

method: Float
_ruby_finiteQ

"Return true if the receiver is neither a NaN nor an Infinity"

  ^ self _getKind < 3 or:[ self = 0.0]
%

method: Float
_ruby_infiniteQ

self _getKind == 3 ifTrue:[ "is an infinity"
  self > 0.0 ifTrue:[ ^ 1 ].
  ^ -1
].
^ nil
%

method: Float
_rubyAsString

"Returns a String corresponding to the value of the receiver.  Where applicable,
 returns one of the following Strings: 'Infinity', '-Infinity', 'NaN' ."

<primitive: 510>
^ self _primitiveFailed: #_rubyAsString
%
method: Float
_rubyAsFormattedString

"Return a string formatted as specified by the hardcoded array. Note that this is a hack
 to allow Benchmark class to work around the lack of Ruby formatting (gsub!)"

^ self asStringUsingFormat: #(10 6 false)
%

method: Float
_rubyLt: aNumber

"Returns true if the receiver is less than aNumber; returns false otherwise."

<primitive: 118>
^ self _rubyRetry: #'<#1__' coercing: aNumber env: 1"__callerEnvId"
%

method: Float
_rubyLteq: aNumber

"Returns true if the receiver is less than aNumber; returns false otherwise."

<primitive: 121>
^ self _rubyRetry: #'<=#1__' coercing: aNumber env: 1"__callerEnvId"
%

method: Float
_rubyEqual: aNumber

"Returns true if the receiver is equal to aNumber; returns false otherwise."

<primitive: 119>
^ [ | arr |
     arr := aNumber @ruby1:coerce: self. 
	"arr is { coercedSelf . coercedNum }"
      (arr at: 1) @ruby1:==: (arr at: 2)
  ] onSynchronous: Exception do:[:ex|
    aNumber @ruby1:==: self 
  ]
%

method: Float
_rubyRaisedTo: aNumber

^ self _mathPrim: aNumber opcode: 4
%

! parser support
classmethod: Float
fromReadStreamInts: anArray

"anArray should be an Array of size two holding SmallIntegers,
 each representing 4 bytes of the IEEE binary result. "

^ self _mathPrim: anArray opcode: 0
%

method: Float
_rubyRetry: aSelector coercing: aNumber env: envId
  "aSelector includes the ruby selector suffix.
   send sites are ruby_selector_suffix dependent"
| arr  |
aNumber _isNumber ifFalse:[
  ArgumentTypeError signal:'cannot coerce a ',  aNumber class name  , ' to a Number'
].
arr := self @ruby1:coerce: aNumber.  "coerce aNumber to a Float"
"arr is { coercedNum . coercedSelf} "

^ (arr at: 2) with: (arr at: 1) perform: aSelector env: envId 
%

!----------------------------------------------

method: Number
_rubyRetry: aSelector coercing: aNumber env: envId
  "aSelector includes the ruby selector suffix.
   send sites are ruby_selector_suffix dependent"
^ [ | arr  |
    arr := aNumber @ruby1:coerce:  self .  
    "arr is { coercedSelf. coercedNum }"
    (arr at: 1) with: (arr at: 2) perform: aSelector env: envId 
  ] onSynchronous: AbstractException do:[:ex |
    ex return:( ArgumentTypeError signal:'numeric coercion failure')
  ]
%

! _rubyTo:by:do:  deleted ,  step implemented in Numeric.rb, Integer.rb




