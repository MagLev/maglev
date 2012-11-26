!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!=========================================================================

set class VariableContext

!  additional methods  for VariableContext to support Ruby 

run
  "allow inherit from Object to support IRB implementation"
  " following line fail may raise an Lookup Error "
[ VariableContext removeSelector:#at:put: ]
  on: LookupError do: [ :exception | ].
true
%
