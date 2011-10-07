!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!  additional methods  for ProfMonitorEntry to support Ruby 
!=========================================================================

set class ProfMonitorEntry

! adding additional methods , don't 'removeallmethods'

category: 'Ruby support'

method:
asStringWidth: nameWidth
  | str mth |
  mth := cmethod .
  mth == #GCI ifTrue:[
    str := #GCI asString width: nameWidth .
  ] ifFalse:[ | mthCls rcvCls envId |
    envId := mth environmentId .
    envId == 0 ifTrue:[  "a Smalltalk method"
      str := mth _classAndSelectorNameWidth: nameWidth .
      mth inClass ~~ (rcvCls := rcvrClass) ifTrue:[
        str add: ' [' ; add: rcvCls name ; add: ']' .
      ].
    ] ifFalse:[
      str := mth _classNameAndRubySelectorWidth: nameWidth .
    ]
  ].
  ^ str
%

