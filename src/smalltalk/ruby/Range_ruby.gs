!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: Range_ruby.gs 25988 2011-06-14 17:55:54Z stever $
!
!  Methods for class Range 
!=========================================================================

set class Range
removeallmethods
removeallclassmethods

category: 'Accessing'

method:
excludeEnd
  ^ excludeEnd
%

method: 
limit

 "Return the limit for an iteration of the C style
    for (int j = begin ; j < limit; j += by) "

  ^ excludeEnd ifTrue:[ to ] ifFalse:[ to + by ]
%

category: 'Comparing'
method: 
= aRange

"Returns true if the receiver is equal to the argument, false otherwise."

self == aRange ifTrue:[ ^ true ].

(aRange _isRange) ifFalse:[ ^ false ].
^ (aRange _from = from and:[ aRange limit = self limit ])
   and:[ aRange increment = by ]
%

method: 
hash

"Returns some Integer related to the contents of the receiver.  If two objects
 compare equal (=) to each other, the results of sending hash to each of those
 objects must also be equal."

^ (from + self limit + by) hash
%

! do: deleted

category: 'Instance Creation'
classmethod: 
from: aStart to: anEnd

^ self basicNew _from: aStart to: anEnd by: 1 
%

classmethod: 
from: aStart limit: anEnd

^ self basicNew _from: aStart limit: anEnd by: 1 
%

classmethod: 
from: aStart to: anEnd by: anIncrement

^ self basicNew _from: aStart to: anEnd by: anIncrement
%

category: 'Private'
method:
_from: aStart to: anEnd by: anInc

  from := aStart .
  to := anEnd .
  by := anInc .
  excludeEnd := false 
%

method:
_from: aStart limit: anEnd by: anInc

  from := aStart .
  to := anEnd .
  by := anInc .
  excludeEnd := true
%

method:
rubyPrivateSize
  ^ 4 "Hide smalltalk instance variables from ruby (marshal)"
%
      
