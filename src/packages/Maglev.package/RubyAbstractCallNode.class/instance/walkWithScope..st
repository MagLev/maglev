*maglev-runtime
walkWithScope: aScope
  "subclasses which contain children 
     not returned by either argNodes or receiverNode methods
   must reimplement walkWithScope: "
  | lst mth knd nam cst | 
  self receiverNode walkWithScope: aScope .
  lst := self argNodes .
  self walkCallArgs: lst withScope: aScope .
  self evalLexPathArg ifNotNil:[ :lp |
    (argsList :={ lp }) addAll: lst .  
  ] ifNil:[
    argsList := lst .
  ].
  mth := (cst := RubyCompilerState current) topMethodDefOrNil.  
  knd := SpecialSelectorKinds at: (nam := self methodName) otherwise: 0 .
  "(nam at:1 equals:'__fixed_instvars') ifTrue:[ self pause ].  "
  knd == 0 ifFalse:[
      knd == 1 ifTrue:[ 
         mth ifNotNil:[ | sel |
            sel := mth selectorPrefix . 
            ((nam == #initialize and:[ sel _at:1 equals:'new' ]) or:[ sel _at:1 equals:'__']) ifTrue:[
             "implementations of  #new  and internal methods can invoke private #initialize "
             bypassProt := true .
           ]
        ].       
        nam == #initialize_copy ifTrue:[ bypassProt := true ].
      ] ifFalse:[
         knd == 2 ifTrue:[  
           mth ifNotNil:[ mth setSendsBinding "for an eval" ]
               ifNil:[ self error:'inconsistent special call node kind' ].
         ] ifFalse:[  
           knd == 3 ifTrue:[
             "fixed instvars defn"
             self walkFixedInstVars: lst forClass: mth .
           ] ifFalse:[ self error: 'invalid special call node kind'  ].
      ].
    ].
  ].

