!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: DateTime_ruby.gs 25988 2011-06-14 17:55:54Z stever $
!
!  additional methods  for DateTime to support Ruby 
!=========================================================================

++++ OBSOLETE FILE

set class DateTime

! adding additional methods , don't 'removeallmethods'

category: 'Ruby support'

method:
asFloat
 "Note, conversion from Smalltalk 1901 epoch to Ruby 1970 epoch obtained from 
   run
   (DateTime newGmtWithYear:1970 month:1 day: 1 hours:0 minutes:0 seconds:0)
   asMillisecondsGmt
   %
   2177452800000
 " 
 ^ (self asMillisecondsGmt"since 1901" - 2177452800000"to 1970 epoch") asFloat / 1000.0
%

method:
httpdate
 "Returns a string which represents the time as rfc1123-date of HTTP-date 
  defined by RFC 2616:
     day-of-week, DD month-name CCYY hh:mm:ss GMT
   Note that the result is always UTC (GMT).
 Example from rfc2616 :
     Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
 "

| res parts addTwoDigits |
addTwoDigits := [ :str :anInt | 
  anInt < 10 ifTrue:[ str add: $0 ].
  str addAll: anInt asString 
].
parts := self asPartsGmt "year , month, day , H, M, S" .
res := String new .
res addAll:  (#( 'Sun, ' 'Mon, ' 'Tues, ' 'Wed, ' 'Thu, ' 'Fri, ' 'Sat, ' ) 
                  at: self dayOfWeekGmt) .
addTwoDigits value: res value: (parts at: 3) .
res addAll: (#(' Jan ' ' Feb ' ' Mar ' ' Apr ' ' May ' ' Jun ' ' Jul ' ' Aug ' 
		' Sep ' ' Oct ' ' Nov  ' ' Dec ') at: (parts at: 2))  ;
  addAll: (parts at: 1) asString ; add: $  .
addTwoDigits value: res value: (parts at:4) .  
res add: $: .
addTwoDigits value: res value: (parts at:5) .  
res add: $: .
addTwoDigits value: res value: (parts at:6) .  
res addAll: ' GMT' .
^ res
%

