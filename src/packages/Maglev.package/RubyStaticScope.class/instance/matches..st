*maglev-runtime
matches: other
	other species = self species ifFalse: [^ false].
	requiredArgs = other requiredArgs ifFalse: [^ false].
	self extraArgs = other extraArgs ifFalse: [^ false].
	self variableNameKeys = other variableNameKeys ifFalse: [^ false].
	^ true

