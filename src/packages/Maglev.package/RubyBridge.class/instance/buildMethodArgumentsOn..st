(as yet unclassified)
buildMethodArgumentsOn: irNode
    1 to: fromNargs  do: [:i | irNode appendArg: (self argLeaf: i)].
    fromRestArg ifTrue: [irNode appendArg: self restLeaf ].
    fromBlockArg ifTrue: [irNode appendArg: self blockLeaf]

