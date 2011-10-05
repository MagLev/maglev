
doit
Notification subclass: 'RubyDynamicVariable'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Tools'
	options: #()

%

set class RubyDynamicVariable
removeallmethods
removeallclassmethods

set class RubyDynamicVariable class
category: '*maglev-runtime'
method:
use: anObject during: aBlock
    ^ aBlock
        onException: self
        do: [:n | n resume: anObject]

%


set class RubyDynamicVariable class
category: 'as yet unclassified'
method:
value
	"self isInvariant ifFalse:[ self immediateInvariant ]."  " should not be needed"
	^ self signal

%

