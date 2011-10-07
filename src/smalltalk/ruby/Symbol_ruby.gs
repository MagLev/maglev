!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: Symbol_ruby.gs 26189 2011-07-18 17:02:57Z otisa $
!
!   
!=========================================================================

!  additional methods  for Symbol to support Ruby 

category: 'Ruby support'
classmethod: Symbol
_rubyAllSymbols
  
| allSyms |
allSyms := ((AllUsers userWithId:'SymbolUser') resolveSymbol: #AllSymbols) value.
^ Array withAll: allSyms keys .
%

method: Symbol
describeClassName

self size == 0 ifTrue:[ ^ String withAll:'a(unnamed)' ].
^ super describeClassName 
%

method: Symbol
_rubyBasicDup

  ^ self
%

! fix Trac 806
method: Symbol
_unscheduleProcess: aGsProcess

  ^ self "do nothing, receiver usually #IoSelect"
%
! fix Trac 806
method: Symbol
_changePriority: aGsProcess from: oldPriority

  ^ self "do nothing, receiver usually #IoSelect"
%

!=========================================================================
method: DoubleByteSymbol
_rubyBasicDup

  ^ self
%

method: QuadByteSymbol
_rubyBasicDup

  ^ self
%

