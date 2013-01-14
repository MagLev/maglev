as yet unclassified
parseTreePort
	|env|
	env := System gemEnvironmentVariable: 'PARSETREE_PORT'.
	^ env ifNil: [2001] ifNotNil: [env asNumber]

