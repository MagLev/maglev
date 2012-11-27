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
  start := self rubyBeginForSize: aLength.
  stop := self rubyEndForSize: aLength.
  start to: stop by: by do: aBlock.
%

method:
isInterval
  ^ true
%

method:
rubyBeginForSize: aSize
  ^ self begin < 0
    ifTrue: [ self begin + aSize ]
    ifFalse: [ self begin ]
%

method:
rubyEndForSize: aSize
  ^ self end < 0
    ifTrue: [ self end + aSize ]
    ifFalse: [ self end ]
%


method:
rubyReplaceIn: aString with: aReplacement
  | aStringsSize end |
  aStringsSize := aString _rubySize.
  end := (self rubyEndForSize: aStringsSize).
  end >= aStringsSize ifTrue: [ end := aStringsSize - 1].
  ^ aString _rubyReplaceFrom: (self rubyBeginForSize: aStringsSize) 
            to: end
            with: aReplacement
%

