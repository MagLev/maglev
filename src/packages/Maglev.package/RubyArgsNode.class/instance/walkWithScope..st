*maglev-runtime
walkWithScope: aScope
   | n args opts |  
    (args := arguments) ifNotNil:[ args walkWithScope: aScope].
    (opts := optArgs) ifNotNil:[ opts walkOptionalArgsLhs: aScope].
    (n := restArgNode) ifNotNil:[ aScope locationForName: n identifier].
    (n := blockArgNode) ifNotNil:[ (aScope addIncomingBlock: n name) ]. 
    aScope
        requiredArgs: ( args ifNil: [0] ifNotNil: [ args list size]);
        restArg: restArg.
    opts ifNotNil:[ opts walkOptionalArgsRhs: aScope].

