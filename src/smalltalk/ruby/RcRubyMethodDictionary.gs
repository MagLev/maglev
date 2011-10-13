!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: RcRubyMethodDictionary.gs 25988 2011-06-14 17:55:54Z stever $
!=========================================================================
set class RcRubyMethodDictionary

+++ file no longer used

removeallmethods
removeallclassmethods

category: 'Updating'
method: 
_keyCollisionOk
  "RC replay allows two sessions to write to same key ,
   last one to commit wins."
  ^ true
%

