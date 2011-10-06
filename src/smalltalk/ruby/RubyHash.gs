!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: Hash_ruby.gs 21313 2009-02-23 17:10:30Z otisa $
!
!=========================================================================
set class RubyHash

removeallmethods
removeallclassmethods

category: 'Ruby support'

classmethod:
_basicNew: aSize
  "aSize converted to near-by prime and used to initialize @tableSize.
   also initializes @collisionLimit, @numCollisions, @numElements,
     @defaultValue = nil , @defaultIsBlock = false .
   varying instVars initialized to remoteNil"

<primitive: 861>
aSize _validateClass: SmallInteger .
self _primitiveFailed: #_basicNew: args: { aSize } .
^ nil
%

category: 'Smalltalk behavior'
classmethod:
new
  ^ self @ruby1:new
%

classmethod:
new: aSize
  ^ self @ruby1:__new: aSize
%

method:
at: aKey

  ^ self @ruby1:__atkey: aKey
%

method:
at: aKey put: aValue

  ^ self @ruby1:__atkey_put: aKey _: aValue
%

category: 'Ruby support'

classmethod: 
rubyPrivateInstSize
^ RubyHash instSize 
%

method:
rubyPrivateSize
^ 6 "inline  RubyHash instSize"
%

method:
fillFrom: index1 resizeTo: newSize with: anObject

"If newSize > 0, 
   change size of receiver to newSize and
   store anObject into instVars 1..newSize 
 else 
   store anObject into instVars 1..(0 - newSize)       
"
<primitive: 607>
index1 _validateClass: SmallInteger .
newSize _validateClass: SmallInteger .
(index1 > newSize abs )
  ifTrue:[ index1 _error: #rtErrBadCopyFromTo args: { newSize }].

(index1 < 1) ifTrue:[ self _errorIndexOutOfRange: index1].
self _primitiveFailed: #fillFrom:resizeTo:with: 
     args: { index1 . newSize . anObject }
%

method:
_primeTableSize: aSize
  "Return an Array { tableSize . collisionLimit }
   containing values to use for resizing receiver to specified length.
   same logic for converting aSize to tableSize as prim 861"
<primitive: 863>
aSize _validateClass: SmallInteger .
self _primitiveFailed: #_primeTableSize: args: { aSize }
%


