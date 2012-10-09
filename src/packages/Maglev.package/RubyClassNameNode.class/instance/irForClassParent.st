*maglev-runtime
irForClassParent
  "if the node is of form A::B , result is IR for
   constant evaluation up to the last ::  ,
   else result is IR for self 
  "
  isColon3 == true ifTrue:[ ^ GsComLiteralNode newObject: Object  ]. 
  ^ leftNode ifNotNil:[ 
     dynamicTypeError ifTrue:[ GsComLiteralNode newNil  ]
                       ifFalse:[  leftNode irNode ].
  ] ifNil:[
    GsComLiteralNode newObject: RubyCompilerState current topRtModule
  ].

