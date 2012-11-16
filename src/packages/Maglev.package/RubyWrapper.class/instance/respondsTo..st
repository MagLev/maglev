testing
respondsTo: aSelector
	"This is Smalltalk reflective API, this will not be passed to the wrappedObject"
	^ (RubyWrapper persistentMethodDictForEnv: 0) keys includes: aSelector

