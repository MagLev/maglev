as yet unclassified
clientData
	"Returns the stored clientData as a runnable copy"
	clientData ifNil: [^ nil].
	^ self
		_convertedClientData: clientData
		from: RubyPersistableCompilerState
		and: RubyPersistableCompilerStack
		to: RubyCompilerState
		and: RubyCompilerStack
