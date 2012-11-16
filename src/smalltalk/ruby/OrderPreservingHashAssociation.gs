!
! From ! GEMSTONE: 3.1.0.1, Wed Nov  7 12:51:14 2012 matthias private build; IMAGE: GemStone/S64 v3.1.0.1 kernel classes filein completed at 10/10/2012 00:53:34

! 

! On November 9, 2012, 4:41:56 PM
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

! ------------------- Class comment for OrderPreservingHashAssociation
run
OrderPreservingHashAssociation comment: 
''.
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

   "Return the value of the instance variable 'index'."
   ^index
%
category: 'Updating'
set compile_env: 0
method: OrderPreservingHashAssociation
index: newValue

   "Modify the value of the instance variable 'index'."
   index := newValue
%
category: 'Accessing'
set compile_env: 0
method: OrderPreservingHashAssociation
key

   "Return the value of the instance variable 'key'."
   ^key
%
category: 'Updating'
set compile_env: 0
method: OrderPreservingHashAssociation
key: newValue

   "Modify the value of the instance variable 'key'."
   key := newValue
%
category: 'Accessing'
set compile_env: 0
method: OrderPreservingHashAssociation
next

   "Return the value of the instance variable 'next'."
   ^next
%
category: 'Updating'
set compile_env: 0
method: OrderPreservingHashAssociation
next: newValue

   "Modify the value of the instance variable 'next'."
   next := newValue
%
category: 'Accessing'
set compile_env: 0
method: OrderPreservingHashAssociation
previous

   "Return the value of the instance variable 'previous'."
   ^previous
%
category: 'Updating'
set compile_env: 0
method: OrderPreservingHashAssociation
previous: newValue

   "Modify the value of the instance variable 'previous'."
   previous := newValue
%
category: 'Accessing'
set compile_env: 0
method: OrderPreservingHashAssociation
value

   "Return the value of the instance variable 'value'."
   ^value
%
category: 'Updating'
set compile_env: 0
method: OrderPreservingHashAssociation
value: newValue

   "Modify the value of the instance variable 'value'."
   value := newValue
%
run
OrderPreservingHashAssociation category: 'Globals'.
true
%
