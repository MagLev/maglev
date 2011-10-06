!=========================================================================
! Copyright (C) VMware, Inc. 1986-2011.  All Rights Reserved.
!
! $Id: auditrubyimage.gs 25988 2011-06-14 17:55:54Z stever $
!
! Description -  Object audit and garbage collect for use in filein.
!
!=========================================================================

! Start a admin and reclaim Gems so reclaimAll can complete

expectvalue true
run
SystemRepository markForCollection
%

expectvalue true
run
SystemRepository auditWithLimit:50000
%
