!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!  methods for CFunction and related classes to support Ruby
!=========================================================================

category: 'Ruby support'
!-----------------------------------
set class CFunction
classmethod:
_addRubyPrimTypes

"executed once during slowrubyimage build step .
 Add ruby primitive types to the ArgTypesDict, for use by
 varArgs support in the primitives."
| d |
d := ArgTypesDict .
d at: #char  put: (d at: #int8 );
  at: #uchar put: (d at: #uint8) ;
  at: #short put: (d at: #int16) ;
  at: #ushort put: (d at: #uint16) ;
  at: #int   put: (d at: #int32) ;
  at: #long   put: (d at: #int64) ;
  at: #uint   put: (d at: #uint32) ;
  at: #ulong   put: (d at: #uint64) ;
  "double, float not yet supported for varArgs"
  at: #size_t   put: (d at: #int64) ;
  at: #long_long  put: (d at: #int64) ;
  at: #ulong_long put: (d at: #uint64) ;
  at: #pointer put: (d at: #ptr ) ;
  at: #buffer_out put: (d at: #ptr ) ;
  at: #buffer_in put: (d at: #ptr ) ;
  at: #buffer_inout put: (d at: #ptr ) ;
  at: #string put: (d at: #'char*' ) ;
  at: #const_string put: (d at: #'char*' ) .
%
run
CFunction _addRubyPrimTypes .
true
%

!-----------------------------------
set class CCallout

classmethod:
_rubyNew: argsArray
  | res |
  (res := self _basicNew) 
     _setLibrary: (argsArray at: 1)"cLibrary" ;
     _name: (argsArray at:2)"aName"
        result: (argsArray at:3) "resType"
        args: (argsArray at:4) "argumentTypes"
        varArgsAfter: (argsArray at:5) "usually -1" .
  res _enumsDict: (argsArray atOrNil: 6) .
  ^ res   
%
method:
_enumsDict: aGsMethodDictionary
  untaggedEnumsDict := aGsMethodDictionary
%

method:
signatureString
  | res fn types nTypes |
  types := argTypes ifNotNil:[ 
    ^ super signatureString   "a Normal FFI callout"
  ].
  "handle callout to a Ruby C extension"
  (res := resultType asString) .
  (fn := fName) ifNotNil:[
     nTypes := types size .
     res add: $  ; add: fn ; add: $(  ;
         add: argCounts asString ; add: ' VALUE(s) ';
        add: $) . 
  ].
  ^ res
%

classmethod:
cextNamed: aName cFunc: addressInt nArgs: nArgs

  | res |
  (res := self _basicNew) _cextName: aName cFunc: addressInt nArgs: nArgs .
  ^ res
%

method:
_cextName: aName cFunc: addressInt nArgs: nArgs 
  "library left as nil"
  fName := aName .
  "argTypes left as nil"
  "result and args are all OopType , handled by prim callCextensionWith:"
  resultType := #uint64 .
  cTypes := #uint64 "actually an array of OopType" .
  argCounts := nArgs . "maybe negative, for signature f(int n, VALUE *a, VALUE self)"
  "argTypesDict left as nil."

  self _storeCextAddress: addressInt .
%

method: 
_storeCextAddress: addressInt

"stores addressInt in cData.dlsymResult 
  and sets library := 1  
     to indicate In a C extension .so, not in an FFI .so"
<primitive: 906>
self _primitiveFailed: #_storeCextAddress: args: { addressInt }
%

method:
callCextension: aSelf with: argsArray block: aBlock ex: fakeEx

<primitive: 905>
"invoke the function described by the receiver.  A call to
 this method will have been installed by an rb_define_method()
 from within the C extension's C code during initialization 
 of the library.   The caller must pass fakeEx == nil"
fakeEx ifNotNil:[
  "primitive has detected  exception return from below the C code"
  fakeEx return: nil  . 
] ifNil:[
  argsArray class == Array ifFalse:[
    ArgumentTypeError new name:'argsArray' expectedClass: Array 
	actualArg: argsArray ; signal .
  ].
  argsArray size > 8 ifTrue:[
    ArgumentError new signal: 'maximum of 8 args to a C extension function'
  ].
].
^ self _primitiveFailed: #callCextension:with:block:ex:
       args: { aSelf . argsArray . aBlock . fakeEx }
%

! -----------------------------------------
set class CCallin

classmethod:
_rubyNew: argsArray
  "a ruby primitive"
  | envId |
  envId := 1"__callerEnvId" .
  ^ self name: (argsArray at:1)"aName"
          result: (argsArray at:2)  "resType"
          args: (argsArray at:3) "argumentTypes" 
          envId: envId 
%


! -----------------------------------------

set class CByteArray

classmethod:
fromAddress: anInteger
  ^ self _newFrom: anInteger offset: 0 numBytes: 0 gcFree: 0
%

classmethod:
_vmOwnedWithAll: aString
  "used by Ruby parser"
 
| res argSiz |
res := self _vmOwnedMalloc: (argSiz := aString size) .
argSiz ~~ 0 ifTrue:[
  res copyFrom: aString from: 1 to: argSiz into: 0 .
].
^ res
%

classmethod:
_vmOwnedMalloc: numBytes
 "(freed by in-memory GC, in VM owned memory, memory is zeroed"

^ self _newFrom: nil offset: 0 numBytes: numBytes gcFree: AutoFree_VmOwned
%


method:
_rubyByteAt: anOffset

  "anOffset is zero based . 
   If anOffset < 0 or anOffset >= self size, primitive fails"

<primitive: 823>
^ nil   "offset was out of bounds, return nil"
%
