!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!=========================================================================

set class Repository

!  additional methods  for Repository to support Ruby 

category: 'Ruby Support'
classmethod: 
_loadedClasses: includeModules
  
  ^ SystemRepository _loadedClasses: includeModules
%

category: 'Listing References'
method:
listReferencesForObject: anObject
  ^ (self listReferencesInMemory: (Array with: anObject)) at: 1
%
