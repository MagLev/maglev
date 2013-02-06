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
(VariableContext includesSelector:#at:put:) ifTrue: [
    VariableContext removeSelector:#at:put: .
].
true
%
