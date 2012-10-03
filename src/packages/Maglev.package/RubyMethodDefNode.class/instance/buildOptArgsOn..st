converting
buildOptArgsOn: aNode
   | optargs |
   (optargs := argsNode optArgs)  ifNotNil:
		[ optargs list do:
			[:ea |
			aNode appendStatement: ea irOptArgNode]]

