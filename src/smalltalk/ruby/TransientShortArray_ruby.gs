!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!=========================================================================

set class TransientShortArray

! adding additional methods , don't 'removeallmethods'

category: 'Ruby support'

classmethod:
_withAllShorts: anArray

| res sz | 
sz := anArray size .
res := self _basicNew: sz  .
1 to: sz do:[:n | | v |
  v := anArray at: n .
  v == 0 ifTrue:[ ArgumentError signal:'cannot store zeros into TransientShortArray'].
  v ifNil:[ v := 0 ].
  res _rubyShortAt: n - 1 put: v
].
^ res
%

method:
_rubyParserShortAt: zeroBasedOffset

"Treat receiver as an array of signed 16bit integers and
 fetch value of specified element.  If the element contains
 zero,  nil is returned."

<primitive: 815>
%

method:
_rubyShortAt: zeroBasedOffset put: aValue

"Treat receiver as an array of signed 16bit integers and
 store specified value in cpu native byteorder at specified offset."

<primitive: 816>
%

