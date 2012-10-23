*maglev-runtime
outerDef
  "return the outermost def"
  ^ outerDef ifNotNil:[:o | o == self ifTrue:[ o ] ifFalse: [outerDef] ] 
              ifNil:[ self ]

