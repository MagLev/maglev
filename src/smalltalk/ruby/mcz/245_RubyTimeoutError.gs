
doit
Error subclass: 'RubyTimeoutError'
	instVarNames: #()
	classVars: #( RubyMethProtection)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Tools'
	options: #()

%

set class RubyTimeoutError
removeallmethods
removeallclassmethods

set class RubyTimeoutError class
category: 'as yet unclassified'
method:
comment
   "this class is deprecated"

%


set class RubyTimeoutError class
category: 'as yet unclassified'
method:
timeout: aNumber do: aBlock
	|sem process val|
	sem := Semaphore new.
	process :=
		[val := aBlock value.
		sem signal] fork.
	(sem waitForSeconds: aNumber) ifFalse:
		[process terminate.
		self signal].
	^ val

%

