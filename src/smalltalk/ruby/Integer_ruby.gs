!=========================================================================
! Copyright (C) VMware, Inc. 2008-2012.  All Rights Reserved.
!
! $Id: Integer_ruby.gs 27251 2012-11-26 17:55:54 Nicco $
!
!  Methods for class Range 
!=========================================================================

set class Integer

category: 'Ruby support'

method:
_rubyChr
  | stringForRuby |
  stringForRuby := '' forRuby.
  stringForRuby add: self asCharacter.
  ^ String fromForRuby: stringForRuby
%


