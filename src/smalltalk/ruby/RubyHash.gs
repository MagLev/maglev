!=========================================================================
! Copyright (C) VMware, Inc. 2008-2012.  All Rights Reserved.
!
! RubyHash.gs: Smalltalk implementation of the Ruby hash table.
!
!=========================================================================
!

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
newWithNextSizeFor: size
	|hash nextSize|
	size == 7 ifTrue: [nextSize := 13].
	size == 13 ifTrue: [nextSize := 23].
	size == 23 ifTrue: [nextSize := 47].
	size == 47 ifTrue: [nextSize := 83].
	size == 83 ifTrue: [nextSize := 163].
	size == 163 ifTrue: [nextSize := 317].
	size == 317 ifTrue: [nextSize := 631].
	size == 631 ifTrue: [nextSize := 631].
	hash := self _primBasicNew: nextSize.
	hash initHash.
	^ hash
%
category: 'Instance creation'
set compile_env: 0
classmethod: RubyHash
_basicNew: aSize
	|hash|
	hash := self _primBasicNew: aSize.
	^ hash
%
category: 'Accessing'
set compile_env: 0
classmethod: RubyHash
hashTableSize
	^ 7
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
hash
  ^ self @ruby1:hash
%
category: 'Instance creation'
set compile_env: 0
classmethod: RubyHash
new
	|hash|
	hash := self _primBasicNew: self hashTableSize.
	hash initHash.
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
category: 'Ruby support'
set compile_env: 0
method: RubyHash
rubyPrepareMarshal
  self _basicSize: self class hashTableSize.
  self initHash.
  self autoRehash: true.
  default := nil.
  self defaultProc: nil.
  self isIdentityHash: false.
%
category: 'Ruby support'
set compile_env: 0
method: RubyHash
rubyPrivateSize
  ^ 10
%
category: 'Basic hash'
set compile_env: 0
method: RubyHash
at: aKey
	self hasNestedHashes
		ifTrue: [(self _at: (self hashSlotIndexFor: aKey)) 
			ifNotNil: [:hash | ^ hash at: aKey]]
		ifFalse: [
			self bitmask = 0 
				ifTrue: [^ self setAt: aKey].
			^ self chainingAt: aKey].
	self @ruby1:__key_error: aKey.
%
category: 'Basic hash'
set compile_env: 0
method: RubyHash
at: aKey put: aValue
	self hasNestedHashes ifTrue: [|index|
		index := self hashSlotIndexFor: aKey.
		(self _at: index) ifNil: [self _at: index put: self newNextHash].
		^ (self _at: index) at: aKey put: aValue]
	ifFalse: [self bitmask = 0 ifTrue: [^ self setAt: aKey put: aValue].
		^ self chainingAt: aKey put: aValue].
%
category: 'Accessing'
method: RubyHash
autoRehash
	autoRehash ifNil: [self autoRehash: true].
   ^ autoRehash
%
category: 'Updating'
set compile_env: 0
method: RubyHash
autoRehash: newValue
   autoRehash := newValue.
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
bitmask
	^ bitmask
%
category: 'Hash set'
set compile_env: 0
method: RubyHash
maxBitmask
	^ 47096
%
category: 'Hash set'
set compile_env: 0
method: RubyHash
setAt: aKey
	^ (self _at: 1) detect: [:assoc |
		self equals: aKey with: assoc key] value.
%
category: 'Hash set'
set compile_env: 0
method: RubyHash
setAt: aKey put: aValue
	|assoc|
	assoc := (self _at: 1) 
		detect: [:ass |	self equals: aKey with: ass key]
		ifNone: [|ass| ass := self newAssociation: aValue at: aKey atIndex: 0.
      self size: self size + 1. 
			(self _at: 1) add: ass].
	^ aValue
%
category: 'Hash set'
set compile_env: 0
method: RubyHash
setRemoveKey: aKey
	|association|
	(self _at: 1) 
		detect: [:ass |	self equals: aKey with: ass key]
		ifNone: [self @ruby1:__key_error: aKey. ^ nil].
	association := (self _at: 1) removeAllSuchThat: [:assoc |
		self equals: aKey with: assoc key].
	association previous next: association next.
	association next previous: association previous.
	^ association value
%
category: 'Updating'
set compile_env: 0
method: RubyHash
bitmask: aBitmask
	bitmask := aBitmask.
%
category: 'Hash chaining'
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
toSetHash
	|set|
	set := Set new.
	(1 to: self hashTableSize) do: [:index |
		set add: (self _at: index)].
	self _at: 1 put: set.
	self bitmask: 0.
%
category: 'Hash chaining'
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
category: 'Hash chaining'
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
category: 'Hash chaining'
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
category: 'Hash chaining'
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
category: 'Basic hash'
set compile_env: 0
method: RubyHash
checkRehash
	|preAutoRehash|
	preAutoRehash := self autoRehash.
	((self hasNestedHashes not and: self autoRehash)
		and: [self fillFactor > self maximumFillFactor]) ifTrue: [
			self bitmask = self maxBitmask
				ifTrue: [self toSetHash] 
				ifFalse: [self autoRehash: false. 
					self rehash. 
					self fillFactor > self maximumFillFactor ifTrue: [self toNestedHash]]].
	self autoRehash: preAutoRehash.
%
category: 'Basic hash'
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
   ^ defaultProc
%
category: 'Updating'
set compile_env: 0
method: RubyHash
defaultProc: newValue
   defaultProc := newValue.
%
category: 'Basic hash'
set compile_env: 0
method: RubyHash
do: aBlock
	|association|
	association := self head next.
	[association ~~ self tail] whileTrue: [
		aBlock value: association value.
		association := association next].
%
category: 'Basic hash'
set compile_env: 0
method: RubyHash
keysAndValuesDo: aBlock
	|association|
	association := self head next.
	[association ~~ self tail] whileTrue: [
		aBlock value: association key value: association value.
		association := association next].
%
category: 'Basic hash'
set compile_env: 0
method: RubyHash
equals: aKey with: anotherKey
  self isIdentityHash
  ifTrue: [^ aKey @ruby1:equal?: anotherKey]
  ifFalse: [^ aKey @ruby1:eql?: anotherKey].
%
category: 'Basic hash'
set compile_env: 0
method: RubyHash
fillFactor
	^ self occupiedSlots / self hashTableSize
%
category: 'Basic hash'
set compile_env: 0
method: RubyHash
hashSlotIndexFor: aKey
	isIdentityHash 
		ifTrue: [ ^ ((aKey @ruby1:object_id) bitAnd: self bitmask) \\ self hashTableSize + 1]
		ifFalse: [ ^ ((aKey @ruby1:hash) bitAnd: self bitmask) \\ self hashTableSize + 1].
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
hashTableSize
	^ self _basicSize
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
   head ifNil: [self initDeque].
   ^ head
%
category: 'Updating'
set compile_env: 0
method: RubyHash
head: newValue
   head := newValue.
%
category: 'Instance creation'
set compile_env: 0
method: RubyHash
initHash
	self hasNestedHashes: false.
	self initDeque.
	self bitmask: 32767.
	self occupiedSlots: 0.
	self size: 0.
%
category: 'Instance creation'
set compile_env: 0
method: RubyHash
initDeque
	self head: OrderPreservingHashAssociation new.
	self tail: OrderPreservingHashAssociation new.
	self tail next: self tail.
	self head previous: self head.
	self tail previous: self head.
	self head next: self tail.
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
isIdentityHash
   ^ isIdentityHash
%
category: 'Updating'
set compile_env: 0
method: RubyHash
isIdentityHash: newValue
  	isIdentityHash := newValue.
	self hasNestedHashes ifTrue: [
		(1 to: self hashTableSize) do: [:index | (self _at: index) isIdentityHash: true]].
%
category: 'Basic hash'
set compile_env: 0
method: RubyHash
maximumFillFactor
	^ 0.7
%
category: 'Basic hash'
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
category: 'Basic hash'
set compile_env: 0
method: RubyHash
newNextHash
	|hash|
	hash := self class newWithNextSizeFor: self hashTableSize.
	hash isIdentityHash: self isIdentityHash.
	hash bitmask: self nextBitmask.
	hash head: self head.
	hash tail: self tail.
	^ hash
%
category: 'Basic hash'
set compile_env: 0
method: RubyHash
nextBitmask
	^ self bitmask + 2047
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
occupiedSlots
   ^ occupiedSlots
%
category: 'Updating'
set compile_env: 0
method: RubyHash
occupiedSlots: aNumber
	occupiedSlots := aNumber.
%
category: 'Basic hash'
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
category: 'Basic hash'
set compile_env: 0
method: RubyHash
rehashPutAssociation: anAssociation
	|index innerHash|
	index := self hashSlotIndexFor: anAssociation key.
	(self _at: index) ifNil: [self _at: index put: self newNextHash].
	innerHash := self _at: index.
	innerHash chainingPutAssociation: anAssociation.
	innerHash size: innerHash size + 1.
%
category: 'Basic hash'
set compile_env: 0
method: RubyHash
removeKey: aKey
	self hasNestedHashes
		ifTrue: [|slot| slot := self _at: (self hashSlotIndexFor: aKey).
			slot ifNil: [self @ruby1:__key_error: aKey]
				ifNotNil: [^ slot removeKey: aKey]]
		ifFalse: [self bitmask = 0 ifTrue: [^ self setRemoveKey: aKey].
			^ self chainingRemove: aKey].		
%
category: 'Basic hash'
set compile_env: 0
method: RubyHash
size
	self hasNestedHashes 
		ifTrue: [|s| s := 0. (1 to: self hashTableSize) do: [:index |
			(self _at: index) ifNotNil: [:hashTable | s := s + hashTable size]].
			^ s]
		ifFalse: [^ size].
%
category: 'Updating'
set compile_env: 0
method: RubyHash
size: newValue
   size := newValue.
%
category: 'Accessing'
set compile_env: 0
method: RubyHash
tail
   tail ifNil: [self initDeque].
   ^ tail
%
category: 'Updating'
set compile_env: 0
method: RubyHash
tail: newValue
   tail := newValue.
%
category: 'Updating'
set compile_env: 0
method: RubyHash
toIdentityHash
	self isIdentityHash: true.
%
category: 'Hash nesting'
set compile_env: 0
method: RubyHash
toNestedHash
	|elements|
	elements := Set new.
	(1 to: self hashTableSize) do: [:index |
		elements add: (self _at: index).
		self _at: index put: nil.
		self size: self size - 1].
	self hasNestedHashes: true.
	elements do: [:assoc |
		self hasNestedHashes ifTrue: [self rehashPutAssociation: assoc]].
%
category: 'Ruby support'
set compile_env: 0
method: RubyHash
_rubyEachPair: aBlock
	self keysAndValuesDo: [:key :value |
		self @ruby1:__do_pair: aBlock _: key _: value].
%
doit
RubyHash category: 'Ruby support'.
true
%
