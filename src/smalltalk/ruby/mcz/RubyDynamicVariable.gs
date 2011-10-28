
set class RubyDynamicVariable
category: '*maglev-runtime'
classmethod:
use: anObject during: aBlock
    ^ aBlock
        onException: self
        do: [:n | n resume: anObject]

%


set class RubyDynamicVariable
category: 'as yet unclassified'
classmethod:
value
	"self isInvariant ifFalse:[ self immediateInvariant ]."  " should not be needed"
	^ self signal

%

