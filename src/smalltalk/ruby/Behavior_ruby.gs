!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!  Ruby support methods for Behavior
!=========================================================================
set class Behavior

category: 'Ruby support'

method: 
isRubyVirtual

  "Returns  true if receiver is a Ruby virtual class used to
   implement a singleton or an include of a module .
   A Ruby virtual class defines no instVars.
   It is used during environment 1  method lookup for Ruby execution. "

  ^ (format bitAnd: 16r3C000) ~~ 0
%
method: 
isRubyModuleIncludeSelf

  "Return true if receiver is a virtual class used to
   implement   include self   in a Module ."

  ^ (format bitAnd: 16r10000) ~~ 0
%
method: 
isRubyModuleInclude

  "Return true if receiver is a virtual class used to
   implement   include of a Module into another Class or Module"

  ^ (format bitAnd: 16r04000) ~~ 0
%
method: 
isRubyModuleFunctions

  "Return true if receiver is a virtual class used to
   implement   the module_function method in Module."

  ^ (format bitAnd: 16r20000) ~~ 0
%
method: 
isMetaModule
"Returns true if receiver is a Module used to implement either
    include self   in a Module
 or
    the module methods  module for a Module
"
  ^ (format bitAnd: 16r30000) ~~ 0
%
method:
rubyPrimaryCopy
  "If receiver is a Ruby virtual class, return the class of which it
   is a copy, otherwise return the receiver."

  self isRubyVirtual ifTrue:[ ^ primaryCopy ].
  ^ self
%
method:
superClass

"Returns the receiver's superclass,
 ignoring ruby virtual and singleton classes."

 | cls |
 cls := superClass .
 [ true ] whileTrue:[
   cls == nil ifTrue:[ ^ cls ].
   cls isRubyVirtual ifFalse:[ ^ cls  ].
   cls := cls _superclass .
 ]
%

method:
persistentMethodDictForEnv: envId put: aValue
  "aValue should be a GsMethodDictionary, or nil ,
   caller responsible for refreshClassCache 
   For ruby, not a protected method  "

| ofs mds |
(mds := methDicts) _isArray ifFalse:[
  envId == 0 ifTrue:[ 
    methDicts := aValue .
    ^ self
  ].
  mds := { mds }.
  mds objectSecurityPolicy: self objectSecurityPolicy .
  methDicts := mds .
].
ofs := envId*4 + 1 .
mds size < ofs ifTrue:[ mds size: ofs ].
mds at: ofs put: aValue
%

method:
_categoriesForEnv: envId put: aValue
  | ofs cats |
  cats := categorys _isArray ifFalse:[
    envId == 0 ifTrue:[  
      categorys := aValue  .
      ^ self
    ].
    cats := { cats }.
    categorys := cats .
  ].
  ofs := envId + 1 .
  cats size < ofs ifTrue:[ cats size: ofs ]. 
  cats at: ofs put: aValue
%

method:
primaryCopy
  "the primaryCopy instVar is used in session-method dictionary
   logic by method loopkup code in the VM "
  ^ primaryCopy
%

method:
rubyPrivateSize
^ self class instSize
%

method:
rubyPrivateInstSize

  ^ self instSize
%

method:
_isIncludableInSingletons: all
  "Test if receiver is allowable as a source of singleton_methods.
   If all is true, then this includes modules extended by caller."
 self isRubySingletonClass ifTrue: [ ^ true ] .
  all ifTrue: [ ^ self isRubyVirtual ] .
  ^ false .
%

! rubyMethods:protection... moved to mcz

method:
rubyBasicNew

"Returns a new instance of the receiver, with the fixed instance 
  variables defined by the receiver initialized to remoteNil.
 Not for use with byte-format objects."

<primitive: 782>

ArgumentTypeError signal:'rubyBasicNew not allowed'.
self _primitiveFailed: #rubyBasicNew .
self _uncontinuableError
%

method:
rubyBasicNew: aSize

"rubyBasicNew semantics, named instVars initialized to remoteNil,
 varying instVars initialized to nil , 
 such as for user defined Ruby subclasses of Array"

<primitive: 783>
aSize _isSmallInteger ifFalse:[ ArgumentTypeError signal:'expected a Fixnum'].
aSize < 0 ifTrue:[ ArgumentError signal:'size must be >= 0'].
ArgumentTypeError signal:'rubyBasicNew not allowed'.
self _primitiveFailed: #rubyBasicNew: args: { aSize }
%

method:
rubyBasicNew_stBaseClass: aClass

"Return a new instance of the receiver, with fixed instance
 variables defined by aClass initialized to nil, and the remaining
 fixed instance variables initialized to remoteNil. 
 Receiver should be identical to or a subClass of aClass. 
 Not for use in creating byte format objects."
 
<primitive: 795>
ArgumentTypeError signal:'rubyBasicNew not allowed'.
self _primitiveFailed: #rubyBasicNew_stBaseClass: args: { aClass } .
self _uncontinuableError
%

method:
rubyNewCFinalizer_stBaseClass: aClass

"Return a new instance of the receiver, with fixed instance
 variables defined by aClass initialized to nil, and the remaining
 fixed instance variables initialized to remoteNil.
 The new instance is registered with VM for finalization of C data,
 as for primitive 674. 
 Receiver should be identical to or a subClass of aClass.
 Not for use in creating byte format objects."

<primitive: 796>
ArgumentTypeError signal:'rubyNewCFinalizer not allowed'.
self _primitiveFailed: #rubyNewCFinalizer_stBaseClass: args: { aClass } .
self _uncontinuableError
%

method:
_setInstancesPersistentBit: persistBool
| fmt mask |   
mask := GC_NON_PERSISTENT_MASK .
persistBool ifTrue:[
  ((fmt := format) bitAnd: mask ) ~~ 0 ifTrue:[
     format := fmt bitXor: mask "clear instancesNP bit" .
     self _refreshClassCache: false 
  ]
] ifFalse:[
  ((fmt := format) bitAnd: mask ) == 0 ifTrue:[
     format := fmt bitOr: mask "set instancesNP bit" .
     self _refreshClassCache: false
  ]
]
%

method:
_setRubyModulePersistentBit: persistBool

| fmt oldVal mask |  
mask := GC_RubyModuleNP .
oldVal := ((fmt := format) bitAnd: mask ) == 0 . "bit means notPersistent"
oldVal == persistBool ifFalse:[
  persistBool ifTrue:[
    format := fmt bitXor: mask  "clear ModuleNP bit" .
  ] ifFalse:[
    format := fmt bitOr:  mask  "set ModuleNP bit" .
  ].
].
^ oldVal
%

method:
_selectorPrefixesReport: envId
  "topaz browsing support"
  | set arr |
  envId == 0 ifTrue:[ ^ self _selectorsReport: envId  "not ruby"].
  arr := self selectorsForEnvironment: envId . 
  set := IdentitySet new .
  arr do:[ :sel |
    set add: (sel prefixIfRubySelector )
  ].
  ^ SortedCollection withAll: set
%

method:
_setInstancesDbTransientBit: aBool
self _validatePrivilege ifTrue:[
  aBool ifTrue:[
    (self isPointers and:[ self isIndexable not]) ifFalse:[
      ^ ImproperOperation new details:'Only non-indexable pointer objects may be DbTransient';
           object: self ; signal 
    ].
    format := format bitOr: 16r1000 .
  ] ifFalse:[
    format := format bitAnd: (16r1000 bitInvert) 
  ].
  self _refreshClassCache: false .
].
%
