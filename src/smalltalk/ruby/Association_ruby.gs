!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!  additional methods  for Association, SymbolAssociation to support Ruby 
!=========================================================================

category: 'Ruby support'

run
Association _addClassVar: #TrapGlobalAssignment value: nil .
(Association _classVars associationAt:#TrapGlobalAssignment )
  _objectSecurityPolicy: DataCuratorObjectSecurityPolicy .
true
%

classmethod: Association
trapGlobalAssignment: anAssociation
  "for debugging use only, requires uncommenting of code in 
   value: and _value: "
  TrapGlobalAssignment := anAssociation
%

method: Association
_value: aVal

  "self == TrapGlobalAssignment ifTrue:[ self pause ]."
  value := aVal 
%

method: Association
value: aVal

  "self == TrapGlobalAssignment ifTrue:[ self pause ]."
  value := aVal 
%

