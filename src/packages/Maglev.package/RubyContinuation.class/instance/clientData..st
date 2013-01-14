as yet unclassified
clientData: anArray
	"Stores the passed clientData in a persistable fashion"
	clientData := anArray
		ifNotNil: [self
					_convertedClientData: anArray
					from: RubyCompilerState
					and: RubyCompilerStack
					to: RubyPersistableCompilerState
					and: RubyPersistableCompilerStack]
