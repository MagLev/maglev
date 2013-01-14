as yet unclassified
restLeaf
	^ leaves at: #rest ifAbsentPut:
		[GsComVarLeaf new methodArg: #rest argNumber: self numArgs + 1]

