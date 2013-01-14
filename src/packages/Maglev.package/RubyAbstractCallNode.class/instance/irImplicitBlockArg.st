as yet unclassified
irImplicitBlockArg
  | mth |
   mth := RubyCompilerState current topMethodDef comIrMethNode  . 
  ^ mth ifNotNil:[ GsComVariableNode new leaf: mth arguments last ]
        ifNil:[ GsComLiteralNode newNil ]

