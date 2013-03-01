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

! includeRubyModule: moved to .mcz
!  classForConstantLookup moved to .mcz
