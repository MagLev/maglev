!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!  This file is compiled in environment 0, and methods are installed into
!  environment 1 by  Ruby code in  prims.rb .
!=========================================================================

set class RubyTime
removeallmethods
removeallclassmethods

category: 'Documentation'
classmethod:
comment
^ 'Instances of RubyTime represent time since 00:00:00 UTC 01 January 1970.

      microseconds   a SmallInteger, microseconds since 00:00:00 UTC 01 January 1970
      is_gmt         a Boolean
      tm             an Array containing the elements of a    struct tm 

      The elements of the Array for tm , in Ruby terms, are,
         tm[0] is seconds
         tm[1] is minutes
         tm[2] is hours
         tm[3] is mday
         tm[4] is month
         tm[5] is year
         tm[6] is wday
         tm[7] is yday
         tm[8] is isdst
   '
%


category: 'Ruby support'
classmethod:
now

"Provided for Smalltalk instance creation"
^ self new 
%

! classmethod  RubyTime>>new inherited from squeak style implementation in Object
!   which will automatically call initialize.

method
_setMicroseconds
"Store the result of operating system gettimeofday() into microseconds instVar
 of the receiver. Returns the receiver."
<primitive: 759>
self _primitiveFailed: #_setMicroseconds
%

method:
initialize
"initialize a newly created instance."
self _setMicroseconds .
self _setTmArray: false
%

method:
_setTmArray: isGmtBoolean

"Sets the tm and is_gmt instVars in the receiver per the microseconds of the receiver
  and the specified  isGmtBoolean . Returns the receiver."
<primitive: 760>
isGmtBoolean _validateClass: Boolean .
self _primitiveFailed: #_setTmArray: args: { isGmtBoolean }
%

method:
strftime: formatString

"Calls operating system strftime() using receiver's tm Array and formatString"
<primitive: 761>
formatString _validateClass: String .
"possible invalid tm instVar or element of tm"
self _primitiveFailed: #strftime:  args: { formatString }
%

classmethod: 
mktime: argsArray fromGmt: gmtBool

"Calls operating system mktime() , returning
 a SmallInteger containing the value of a time_t "
<primitive: 762>
argsArray _validateClass: Array .
argsArray size < RubyTmArray_SIZE ifTrue:[ 
  argsArray _error: #errArgTooSmall args:{ RubyTmArray_SIZE }
].
gmtBool _validateClass: Boolean .
self _primitiveFailed: #mktime:fromGmt: args: { argsArray . gmtBool }
%
