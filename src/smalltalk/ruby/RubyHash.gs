!
! From ! GEMSTONE: 3.1.0.1, Wed Nov  7 12:51:14 2012 matthias private build; IMAGE: GemStone/S64 v3.1.0.1 kernel classes filein completed at 10/10/2012 00:53:34

! 

! On November 9, 2012, 4:41:41 PM
!
! doit
! Object indexableSubclass: #RubyHash
!	instVarNames: #(  head tail
! 	                  bitmask hasNestedHashes occupiedSlots
! 	                  autoRehash size default isIdentityHash
! 	                  defaultProc)
! 	classVars: #()
! 	classInstVars: #()
! 	poolDictionaries: #()
! 	inDictionary: Globals
! 	options: #( disallowGciStore).
! true
! %

! ------------------- Class comment for RubyHash
! doit
! RubyHash comment: 
! ''
! %

! Remove existing behavior from RubyHash
doit
RubyHash removeAllMethods.
RubyHash class removeAllMethods.
true
%
! ------------------- Class methods for RubyHash
category: 'Instance creation'
set compile_env: 0
classmethod: RubyHash
_primBasicNew: aSize
	<primitive: 861>
	aSize _validateClass: SmallInteger.
	self _primitiveFailed: #_basicNew: args: {aSize}.
	^ nil
%
category: 'Instance creation'
set compile_env: 0
classmethod: RubyHash
_basicNew: aSize
	|hash|
	hash := self _primBasicNew: aSize.
	hash initHash.
	^ hash
%
category: 'Accessing'
set compile_env: 0
classmethod: RubyHash
hashTableSize
	^ 2017
%
category: 'Instance creation'
set compile_env: 0
classmethod: RubyHash
new
	|hash|
	hash := self _basicNew: self hashTableSize.
	hash initialize.
	^ hash
%
! ------------------- Instance methods for RubyHash
category: 'Ruby support'
set compile_env: 0
method: RubyHash
asArray
	|array association|
	association := self head next.
	array := Array rubyBasicNew: self size.
	(1 to: self size) do: [:index | |tuple|
		tuple := Array rubyBasicNew: 2.
		tuple at: 1 put: association key.
		tuple at: 2 put: association value.
		array at: index put: tuple.
		association := association next].
	^ array
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
at: aKey
	self hasNestedHashes
		ifTrue: [^ (self _at: (self hashSlotIndexFor: aKey)) 
			ifNotNil: [:hash | ^ hash at: aKey]]
		ifFalse: [^ self chainingAt: aKey].
	^ nil
%
category: 'Updating'
set compile_env: 0
method: RubyHash
at: aKey put: aValue
	self hasNestedHashes ifTrue: [|index|
		index := self hashSlotIndexFor: aKey.
		(self _at: index) ifNil: [self _at: index put: self newNextHash].
		^ (self _at: index) at: aKey put: aValue]
	ifFalse: [^ self chainingAt: aKey put: aValue].
%
category: 'Accessing'
method: RubyHash
autoRehash

   "Return the value of the instance variable 'autoRehash'."
	autoRehash ifNil: [self autoRehash: true].
   ^autoRehash
%
category: 'Updating'
set compile_env: 0
method: RubyHash
autoRehash: newValue

   "Modify the value of the instance variable 'autoRehash'."
   autoRehash := newValue
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
bitmask
	^ bitmask
%
category: 'Updating'
set compile_env: 0
method: RubyHash
bitmask: aBitmask
	bitmask := aBitmask
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
chainingAt: aKey
	|index|
	index := self chainingFind: aKey.
	(self _at: index)
		ifNil: [self @ruby1:__key_error: aKey]
		ifNotNil: [^ (self _at: index) value].
%
category: 'Updating'
set compile_env: 0
method: RubyHash
chainingAt: aKey put: aValue
	|index|
	index := self chainingFind: aKey.
	(self _at: index) 
		ifNil: [self _at: index put: (self newAssociation: aValue at: aKey atIndex: index).
			self size: self size + 1.
			self occupiedSlots: self occupiedSlots + 1.
			self checkRehash]
		ifNotNil: [(self _at: index) value: aValue].
	^ aValue
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
chainingFind: aKey
	|index|
	index := self hashSlotIndexFor: aKey.
	[|el| el := self _at: index.
		el isNil or: [el ~~ false and: [self equals: el key with: aKey]]]
			whileFalse: [index := index + 1 \\ self hashTableSize + 1].
	^ index
%
category: 'Updating'
set compile_env: 0
method: RubyHash
chainingPutAssociation: anAssociation
	|index|
	index := self hashSlotIndexFor: anAssociation key.
	[|el| el := self _at: index.
		el isNil or: [el ~~ false and: [self equals: el key with: anAssociation key]]]
			whileFalse: [index := index + 1 \\ self hashTableSize + 1].
	self _at: index put: anAssociation.
	self occupiedSlots: self occupiedSlots + 1.
	self checkRehash.
%
category: 'Updating'
set compile_env: 0
method: RubyHash
chainingRemove: aKey
	|index|
	index := self chainingFind: aKey.
	(self _at: index)
		ifNil: [self @ruby1:__key_error: aKey]
		ifNotNil: [|value association|
			association := self _at: index.
			association previous next: association next.
			association next previous: association previous.
			self _at: index put: false.
			self size: self size - 1.
			^ association value].
%
category: 'Updating'
set compile_env: 0
method: RubyHash
checkRehash
	|preAutoRehash|
	preAutoRehash := self autoRehash.
	((self hasNestedHashes not and: self autoRehash)
		and: [self fillFactor > self maximumFillFactor 
		and: [self autoRehash: false. self rehash. self fillFactor > self maximumFillFactor]])
			ifTrue: [self toNestedHash].
	self autoRehash: preAutoRehash.
%
category: 'unknown'
set compile_env: 0
method: RubyHash
clear
	(1 to: self hashTableSize) do: [:index |
		self _at: index put: nil].
	self initHash.
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
defaultProc

   "Return the value of the instance variable 'defaultProc'."
   ^defaultProc
%
category: 'Updating'
set compile_env: 0
method: RubyHash
defaultProc: newValue

   "Modify the value of the instance variable 'defaultProc'."
   defaultProc := newValue
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
do: aBlock
	|association|
	association := self head next.
	[association ~~ self tail] whileTrue: [
		aBlock value: association value.
		association := association next].
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
doKeyValue: aBlock
	|association|
	association := self head next.
	[association ~~ self tail] whileTrue: [
		aBlock value: association key value: association value.
		association := association next].
%
category: 'unknown'
set compile_env: 0
method: RubyHash
equals: aKey with: anotherKey
	self isIdentityHash 
		ifTrue: [^ aKey == anotherKey]
		ifFalse: [^ aKey = anotherKey].
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
fillFactor
	^ self occupiedSlots / self hashTableSize
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
hashSlotIndexFor: aKey
	^ (aKey hash bitAnd: self bitmask) \\ self hashTableSize + 1
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
hashTableSize
	^ 2017
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
hasNestedHashes
	^ hasNestedHashes
%
category: 'Updating'
set compile_env: 0
method: RubyHash
hasNestedHashes: aBoolean
	hasNestedHashes := aBoolean.
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
head

   "Return the value of the instance variable 'head'."
   ^head
%
category: 'Updating'
set compile_env: 0
method: RubyHash
head: newValue

   "Modify the value of the instance variable 'head'."
   head := newValue
%
category: 'unknown'
set compile_env: 0
method: RubyHash
initHash
	| firstAssoc lastAssoc |
	self head: OrderPreservingHashAssociation new.
	self tail: OrderPreservingHashAssociation new.
	self tail next: self tail.
	self head previous: self head.
	self tail previous: self head.
	self head next: self tail.
	self hasNestedHashes: false.
	self isIdentityHash: false.
	self _basicSize: self hashTableSize.
	(1 to: self hashTableSize) do: [:index | ((self _at: index) == _remoteNil) ifTrue: [self _at: index put: nil]].
	self bitmask: 32767.
	self occupiedSlots: 0.
	self size: 0.
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
isIdentityHash

   "Return the value of the instance variable 'isIdentityHash'."
   ^isIdentityHash
%
category: 'Updating'
set compile_env: 0
method: RubyHash
isIdentityHash: newValue

  	"Modify the value of the instance variable 'isIdentityHash'."
  	isIdentityHash := newValue.
	self hasNestedHashes ifTrue: [
		(1 to: self hashTableSize) do: [:index | (self _at: index) isIdentityHash: true]].
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
maximumFillFactor
	^ 0.7
%
category: 'Updating'
set compile_env: 0
method: RubyHash
newAssociation: aValue at: aKey atIndex: anIndex
	| value |
	value := OrderPreservingHashAssociation
		newWithValue: aValue at: aKey atIndex: anIndex previous: self tail previous next: self tail.
	self tail previous next: value.
	self tail previous: value.
	^ value
%
category: 'Updating'
set compile_env: 0
method: RubyHash
newNextHash
	|hash|
	hash := self class new.
	hash isIdentityHash: self isIdentityHash.
	hash bitmask: self nextBitmask.
	hash head: self head.
	hash tail: self tail.
	^ hash
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
nextBitmask
	^ self bitmask + 2047
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
occupiedSlots

   "Return the value of the instance variable 'occupiedSlots'."
   ^occupiedSlots
%
category: 'Updating'
set compile_env: 0
method: RubyHash
occupiedSlots: aNumber
	occupiedSlots := aNumber
%
category: 'Updating'
set compile_env: 0
method: RubyHash
rehash
	|elements|
	self hasNestedHashes ifFalse: [
		elements := Set new.
		self occupiedSlots: 0.
		(1 to: self hashTableSize) do: [:index | |el|
			el := self _at: index.
			(el isNil not and: el ~~ false) ifTrue: [
				elements add: (self _at: index)].
			self _at: index put: nil].
		elements do: [:assoc | self chainingPutAssociation: assoc]].
%
category: 'Updating'
set compile_env: 0
method: RubyHash
rehashPutAssociation: anAssociation
	|index|
	index := self hashSlotIndexFor: anAssociation key.
	(self _at: index) ifNil: [self _at: index put: self newNextHash].
	(self _at: index) chainingPutAssociation: anAssociation.
%
category: 'Updating'
set compile_env: 0
method: RubyHash
removeKey: aKey
	self hasNestedHashes
		ifTrue: [(self _at: (self hashSlotIndexFor: aKey)) removeKey: aKey]
		ifFalse: [self chainingRemove: aKey].
		
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
size
	self hasNestedHashes 
		ifTrue: [|s| (1 to: self hashTableSize) do: [:index |
			(self _at: index) ifNotNil: [:hashTable | s := s + hashTable size]].
			^ s]
		ifFalse: [^ size].
%
category: 'Updating'
set compile_env: 0
method: RubyHash
size: newValue

   "Modify the value of the instance variable 'size'."
   size := newValue
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
tail

   "Return the value of the instance variable 'tail'."
   ^tail
%
category: 'Updating'
set compile_env: 0
method: RubyHash
tail: newValue

   "Modify the value of the instance variable 'tail'."
   tail := newValue
%
category: 'Updating'
set compile_env: 0
method: RubyHash
toIdentityHash
	self isIdentityHash: true.
%
category: 'Updating'
set compile_env: 0
method: RubyHash
toNestedHash
	|elements|
	elements := Set new.
	(1 to: self hashTableSize) do: [:index |
		elements add: (self _at: index).
		self _at: index put: nil].
	self hasNestedHashes: true.
	elements do: [:assoc |
		self hasNestedHashes ifTrue: [self rehashPutAssociation: assoc]].
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
_rubyEachPair: aBlock
	self doKeyValue: [:key :value |
		self @ruby1:__do_pair: aBlock _: key _: value].
%
doit
RubyHash category: 'Globals'.
true
%
