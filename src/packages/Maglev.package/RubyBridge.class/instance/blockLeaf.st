(as yet unclassified)
blockLeaf
    | idx |
    ^ leaves at: #block ifAbsentPut:
        [idx :=  fromRestArg
                ifTrue: [ fromNargs  + 2]
                ifFalse: [ fromNargs + 1].
        GsComVarLeaf new methodArg: #block argNumber: idx]

