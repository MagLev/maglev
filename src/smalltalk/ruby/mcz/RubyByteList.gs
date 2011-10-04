
doit
Object subclass: 'RubyByteList'
	instVarNames: #( begin hash realSize
	                  validHash bytes stringValue)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyByteList
removeallmethods
removeallclassmethods

set class RubyByteList
category: 'as yet unclassified'
method:
= other
	^ other string = self string

%


set class RubyByteList
category: 'accessing'
method:
begin

	 ^ begin

%


set class RubyByteList
category: 'accessing'
method:
begin: aNumber
	begin := aNumber

%


set class RubyByteList
category: 'accessing'
method:
bytes

	 ^ bytes

%


set class RubyByteList
category: 'accessing'
method:
bytes: anArray
	bytes := anArray

%


set class RubyByteList
category: 'accessing'
method:
hash
	^ self string hash

%


set class RubyByteList
category: 'accessing'
method:
hash: aNumber
	hash := aNumber

%


set class RubyByteList
category: 'converting'
method:
irLeaf
	^ GsComLitLeaf new stringLiteral: self string

%


set class RubyByteList
category: 'printing'
method:
printSourceOn: aStream
	aStream nextPutAll: self string printString

%


set class RubyByteList
category: 'accessing'
method:
realSize

	 ^ realSize

%


set class RubyByteList
category: 'accessing'
method:
realSize: aNumber
	realSize := aNumber

%


set class RubyByteList
category: 'printing'
method:
string
	^ (bytes asByteArray copyUpTo: 0) asString

%


set class RubyByteList
category: 'accessing'
method:
stringValue

	 ^ stringValue

%


set class RubyByteList
category: 'accessing'
method:
stringValue: aString
	stringValue := aString

%


set class RubyByteList
category: 'accessing'
method:
validHash

	 ^ validHash

%


set class RubyByteList
category: 'accessing'
method:
validHash: a
	validHash := a

%

