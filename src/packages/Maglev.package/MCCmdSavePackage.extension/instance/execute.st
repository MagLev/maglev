*maglev-override-execution
execute
	"MagLev override, added 'self executeFileOut' call in last block"
	| version v m |
	self checkForNewerVersions == true ifFalse: [^self].
	[version :=  self workingCopy newVersion]
		on: MCVersionNameAndMessageRequest
		do: [:n |
			v := OBTextRequest prompt: 'Please enter a name for your version:' template: n suggestedName.
			[ v == nil ifTrue: [ n resume: nil ].
			  v = (v encodeForHTTP) ] 
				whileFalse: [
					v := OBTextRequest 
						prompt: (v encodeForHTTP) printString, ' contains illegal characters. Please enter a valid name for your version:' 
						template: n suggestedName ].
			m _ OBMultiLineTextRequest prompt: 'Please enter a commit message:'.
			(m ~~ nil and: [ m isEmpty not ])
				ifTrue: [ n resume: {v. m} ].
			n resume: nil].
	version ifNotNil:
		[self repository storeVersion: version.
		self executeFileOut.
		version allDependenciesDo:
			[:dep |
				(self repository includesVersionNamed: dep info name)
					ifFalse: [self repository storeVersion: dep]]].
	self refresh

