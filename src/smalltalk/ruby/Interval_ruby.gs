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
rubyDo: aBlock withLength: aLength
  | start stop |
  start := self begin.
  stop := self end.
  start < 0 ifTrue: [ start := start + aLength].
  stop < 0 ifTrue: [ stop := stop + aLength].
  start to: stop by: by do: aBlock.
%

method:
isInterval
  ^ true
%
