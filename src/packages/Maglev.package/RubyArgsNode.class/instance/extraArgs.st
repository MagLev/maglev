converting
extraArgs
	| i opts |
	i := 0.
	(opts := optArgs) ifNotNil: [i := i + opts list size].
	restArgNode ifNotNil: [i := i + 1].
	blockArgNode ifNotNil: [i := i + 1].
	^ i

