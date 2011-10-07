!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: MatchData.gs 25988 2011-06-14 17:55:54Z stever $
!
!  Smalltalk methods for class MatchData
!=========================================================================

set class  MatchData

removeallmethods
removeallclassmethods

category: 'Private'
classmethod:
new: aSize
 "disallowed, instances are created by Regexp methods "
  self shouldNotImplement: #new:
%
classmethod:
new
 "disallowed, instances are created by Regexp methods "
  self shouldNotImplement: #new
%

method:
nthRegexRef: anOffset
 " called from generated code.
   anOffset is  in the range 1..9 .
   Returns the value of Ruby Global variables $1 to $9   .
   anOffset==0 means return the  Ruby Global $& .
 "
^ self _rubyAt: anOffset
%

! pre_match, post_match, plus_match  all in .rb files now

method
_rubyAt: anOffset

  "Ruby MatchData   [anInt]   .
   Return the specified portion of the match.
   anOffset is zero based.
   anOffset may be negative, in which case, count starting at end (-1 is last element).
  "
  anOffset _isSmallInteger ifTrue:[
    | res cache ofs start limit oneOfs posOffset cacheVal |
    posOffset := anOffset < 0 ifTrue:[ (self size // 2) + anOffset ] ifFalse:[ anOffset ].
    (cache := groupsCache) ifNotNil:[
      posOffset < cache size ifTrue:[
        cacheVal := cache at: posOffset + 1 .
        cacheVal ifNotNil:[
          cacheVal == 0 ifTrue:[ ^ nil ].
          ^ cacheVal .
        ].
      ].
    ].
    ofs := (posOffset * 2) + 1 .
    ofs > self size ifTrue:[ ^ nil ].
    start := self at: ofs .  "zero based"
    limit := self at: ofs + 1 .  "zero based"

   "If start (and end) is -1, that means a nil result rather than the empty string" 
    start == -1 ifTrue:  [ res := nil . cacheVal := 0 ] 
                ifFalse: [ res := inputString copyFrom:start + 1 to: limit.  cacheVal := res ] .
    oneOfs := posOffset + 1 .
    cache ifNil:[
      cache := Array new: oneOfs .
      groupsCache := cache
    ] ifNotNil:[
      cache size: oneOfs .
    ].
    cache at: oneOfs put: cacheVal .
    ^ res .
  ].
  anOffset _isRange  ifTrue:[ | start limit |
    "anOffset is a Range, limit may be <= 0 which indicates to end of matches"
    start := anOffset begin .
    limit := anOffset limit .
    limit <= 0 ifTrue:[ limit := (self size // 2) + limit ] .
    ^ self _rubyAt: start length: (limit - start) .
  ].
  anOffset _error: #rtErrBadArgKind args:{ SmallInteger . Range }
%

method:
_rubyAt: anOffset length: aCount

  "Ruby MatchData   [anOffset,aCount]
  anOffset may be negative, which means anOffset starts from the end (-1 is
  last element).
  "

| res zOffset ofs limitOfs mySize |
res := { } .
mySize := self size .
zOffset := anOffset < 0 ifTrue:[  (mySize // 2) + anOffset ]
                    ifFalse:[ anOffset ].
ofs := (zOffset * 2) + 1 .
limitOfs := ofs + (aCount * 2) .
[ ofs < limitOfs ] whileTrue:[ | start |
  start := self at: ofs .
  start < 0 ifTrue:[
    res add: nil 
  ] ifFalse:[
    res add:( inputString _rubyAt: start length:  (self at: ofs + 1) - start ).
  ].
  ofs := ofs + 2 .
].
^ res
%
