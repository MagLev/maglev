*maglev-runtime
buildConstRef

	globAssoc ifNotNil: [
		globAssoc class == RubyConstantRef ifFalse: [
			^ super buildConstRef "an association bound at compile time, in bootstrap"
		]
	].
	^ globAssoc
