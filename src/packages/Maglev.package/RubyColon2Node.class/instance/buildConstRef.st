*maglev-runtime
buildConstRef 
 | ref  |
 leftNode ifNotNil:[
   ref  := leftNode buildConstRef .
   ref ifNil:[ "lhs not a Colon2 node" 
     ref := { leftNode irNode }
   ].
   ref  add: name 
 ] ifNil: [ | cst lp |
   cst := RubyCompilerState current .
   lp := inInnerDef ifTrue:[ cst outerDefLexPath ]
                   ifFalse:[ cst rtModuleLexPath  ].
   (ref := RubyConstantRef _basicNew )
          name: name c2lexPath: lp evalSelfCls: cst topLexicalSelf   .
 ] .
 ^ ref 

