!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!  Ruby support methods for ObsoleteMetaclass 
!=========================================================================

set class ObsoleteMetaclass

category: 'Ruby support'

method: 
addModuleMethodIfEnabled: aSelector
  "do nothing"
%
method: 
isMetaOrModule
  ^ true
%

