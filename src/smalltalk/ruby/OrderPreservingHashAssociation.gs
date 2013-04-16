!=========================================================================
! Copyright (C) VMware, Inc. 2008-2012.  All Rights Reserved.
!
! OrderPreservingHashAssociation.gs: Smalltalk implementation of the 
! Ruby hash table. An association saves a pointer to the next and 
! to the previous entry in the hash table.
!
!=========================================================================
!

run
Object subclass: #OrderPreservingHashAssociation
	instVarNames: #( key value previous
	                  next index)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals 
	options: #().
true
%
! Remove existing behavior from OrderPreservingHashAssociation
run
OrderPreservingHashAssociation removeAllMethods.
OrderPreservingHashAssociation class removeAllMethods.
true
%
! ------------------- Class methods for OrderPreservingHashAssociation
category: 'unknown'
set compile_env: 0
classmethod: OrderPreservingHashAssociation
newWithValue: aValue at: aKey atIndex: anIndex previous: previousLink next: nextLink
	^ self new
		value: aValue;
		key: aKey;
		index: anIndex;
		previous: previousLink;
		next: nextLink;
		yourself
%
! ------------------- Instance methods for OrderPreservingHashAssociation
category: 'Accessing'
set compile_env: 0
method: OrderPreservingHashAssociation
index
   ^ index
%
category: 'Updating'
set compile_env: 0
method: OrderPreservingHashAssociation
index: newValue
   index := newValue.
%
category: 'Accessing'
set compile_env: 0
method: OrderPreservingHashAssociation
key
   ^ key
%
category: 'Updating'
set compile_env: 0
method: OrderPreservingHashAssociation
key: newValue
   key := newValue.
%
category: 'Accessing'
set compile_env: 0
method: OrderPreservingHashAssociation
next
   ^ next
%
category: 'Updating'
set compile_env: 0
method: OrderPreservingHashAssociation
next: newValue
   next := newValue.
%
category: 'Accessing'
set compile_env: 0
method: OrderPreservingHashAssociation
previous
   ^ previous
%
category: 'Updating'
set compile_env: 0
method: OrderPreservingHashAssociation
previous: newValue
   previous := newValue.
%
category: 'Accessing'
set compile_env: 0
method: OrderPreservingHashAssociation
value
   ^ value
%
category: 'Updating'
set compile_env: 0
method: OrderPreservingHashAssociation
value: newValue
   value := newValue.
%
run
OrderPreservingHashAssociation category: 'Ruby support'.
true
%
