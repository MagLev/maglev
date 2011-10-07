!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!  additional methods  for ProfMonitorTree to support Ruby 
!=========================================================================

set class ProfMonitorTree
!  this class is Maglev::Gprof in Ruby

! adding additional methods , don't 'removeallmethods'

category: 'Ruby support'

classmethod:
monitorIntervalNs: ns block: aBlock

 ^ self monitorBlock: aBlock intervalNs: ns
%
