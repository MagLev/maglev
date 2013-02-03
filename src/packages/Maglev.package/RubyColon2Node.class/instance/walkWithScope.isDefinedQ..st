*maglev-runtime
walkWithScope: aScope isDefinedQ: isQ
  "returns nil always"
  | inBoot cst  |
  inBoot := aScope inBootstrap .
  cst := RubyCompilerState current .
  inInnerDef := cst topMethodDefOrNil ifNotNil:[:md | md isInnerDef ] ifNil:[ false ].
  (self determineDynamic ~~ 0 ) ifTrue:[
     leftNode walkWithScope: aScope isDefinedQ: isQ .
     inBoot ifTrue:[ | leftAssoc ln |
        isQ ifTrue:[ self error:' defined?  not supported during bootstrap'].
        ((ln :=leftNode) isKindOf: RubyColon2Node) ifFalse:[
            self error:'variable on lhs of :: not supported during bootstrap'].
        leftAssoc := ln globalAssoc .
        (leftAssoc isKindOf: Association) ifTrue:[ 
          globAssoc :=  leftAssoc _valueNoAction 
               bootConstantLookup: name env: cst envId .
        ].
        isTypeClass := (name == #Type and: [ln name == #Maglev]).
     ].
  ] ifFalse:[   "left node is nil"
    inBoot ifTrue:[ | assoc ns rtModuStk | 
      rtModuStk := cst rtModuleStack .
      assoc :=  (ns := aScope nameSpace) ifNotNil:[ 
                     ns bootConstantLookup: name env: cst envId ] .
      assoc ifNil:[ 
        self warnDynamicConst: name 
      ] ifNotNil: [  | val key |
        isProcClass :=  (val := assoc _valueNoAction) == RubyProc .
        (inBoot and:[ val isNameSpace ]) ifTrue:[  
          self warnDynamicConst: key .
        ].
      ].
      globAssoc := assoc .
    ] ifFalse:[ 
      isProcClass := name == #Proc  "it may be Proc,   Trac 538"
    ]
  ].
  ^ nil

