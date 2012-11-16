*maglev-runtime
irArgNodes
    | res n |
    res := super irArgNodes .
    (n := superNode) ifNotNil:[ res add: n irNode ].
    (n := fixedIvs) ifNotNil:[ res add: n irNode ]
                   ifNil:[ res add: ( GsComLiteralNode newObject: #())  ].
    ^ res

