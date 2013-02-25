!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!  Ruby support methods for Metaclass3 
!=========================================================================

category: 'Ruby support'
set class Metaclass3

method:
isMetaOrModule
  ^ self isMeta 
%

method:
addModuleMethod: aSelector
  self shouldNotImplement: #addModuleMethod:
%
method:
moduleMethodsModule
  self shouldNotImplement: #moduleMethodsModule
%

method
_instancesPersistent
  ^ (self class format bitAnd: GC_NON_PERSISTENT_MASK ) == 0  
%

method
_setInstancesPersistent: aBool
  self _setInstancesPersistentBit: aBool .
  ^ aBool
%

method:
_setInstancesDbTransient
     <primitive: 2001>
       | prot |
       prot := System _protectedMode .
     [
         self _makeInstancesDbTransient: true.
     ] ensure: [
         prot _leaveProtectedMode
     ].
%

method:
_makeInstancesDbTransient: aBool

<protected>
self _validatePrivilege ifTrue:[
   aBool ifTrue:[
     (self isPointers and:[ self isIndexable not]) ifFalse:[
       ^ ImproperOperation new details:'Only non-indexable pointer 
objects may be DbTransient';
            object: self ; signal
     ].
     format := format bitOr: 16r1000 .
   ] ifFalse:[
     format := format bitAnd: (16r1000 bitInvert)
   ].
   self _refreshClassCache: false .
].

%

! includeRubyModule: moved to .mcz
!  classForConstantLookup moved to .mcz


