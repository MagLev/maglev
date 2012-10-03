parsetree-as yet unclassified
variableNames
	| names n |
	names := { } .
	(n := arguments) ifNotNil:	[ n list do:	[:ea |	names add: ea identifier]].
	(n := optArgs) ifNotNil:[ n list do:[:ea |	names add: ea name]].
	(n := restArgNode) ifNotNil: [names add: n identifier].
	(n := blockArgNode) ifNotNil: 	[names add: n name].
	^ names

