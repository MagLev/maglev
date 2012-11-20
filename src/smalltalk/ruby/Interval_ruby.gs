!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: Interval_ruby.gs 25988 2011-06-14 17:55:54Z stever $
!
!  Methods for class Interval 
!=========================================================================

set class Interval

category: 'Ruby support'

method:
rubyDo: aBlockClosure
  ^ self do: aBlockClosure
%

method:
isInterval
  ^ true
%
