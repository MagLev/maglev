*maglev-runtime
compiledMethodForIR: irNode
 | cm vbs errs text | 
 cm :=  GsNMethod generateFromIR: irNode.
 (cm isKindOf: Array) ifTrue: [
    (vbs := Verbose) ifNotNil:[    self basicLog: cm printString; log: 'Compilation error' ].
    (cm at: 1) class == GsNMethod ifTrue:[
        vbs ifNotNil:[ self basicLog: 'Compile warnings' ].
           ^ cm at: 1.
    ].
    errs := cm at: 2 .
    text := String new .
    errs do:[ :descr | | errNum arg |
       text add: 'compilerError ' ; add: (errNum := descr at: 1) asString ; add: ': ' .
       (arg := descr atOrNil: 3) ifNotNil:[ text add: arg asString ; add: ', ' ].
       (arg := descr atOrNil: 4) ifNotNil:[ text add: arg asString ; add: ', ' ].
       text add: (((Globals at: #GemStoneError) at: #English) atOrNil: errNum ) asString .
     ].
    CompileError new 
       args: { errs . nil "no source". 'no category'. Object. System myUserProfile symbolList.
                'no selector' } ; 
       messageText: text ;
       signal
 ].
 ^ cm

