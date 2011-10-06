!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!=========================================================================
set class RubyThreadGroup
removeallmethods
removeallclassmethods

category: 'Ruby support'
classmethod:
new
  ^ self _basicNew initialize
%

classmethod:
default

  "should be initialized by GsProcess>>initRubyMainThread , 
   called from RubyCompiler ."

  ^ SessionTemps current at: #RubyDefaultThreadGroup otherwise: nil
%

method:
initialize
  closed := false 
%

method:
list

"Return an Array of all instances of GsProcess known to the ProcessScheduler,
  which are in the group defined by the receiver. "

 | set sched |
 set := IdentitySet new .
 (sched := ProcessorScheduler scheduler) _allProcessesInto: set inGroup: self .
 self == self class default ifTrue:[
   sched _allProcessesInto: set inGroup: nil
 ].
 ^ set asArray
%
