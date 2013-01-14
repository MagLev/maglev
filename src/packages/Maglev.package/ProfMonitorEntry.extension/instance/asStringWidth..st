*maglev-runtime
asStringWidth: nameWidth
  | str mth |
  mth := cmethod .
  mth == #GCI ifTrue:[
    str := #GCI asString width: nameWidth .
  ] ifFalse:[ | rcvCls  |
    mth environmentId == 0 ifTrue:[  | forBlock homeMth rubyPrimSel envId|
      "a Smalltalk method, possibly a ruby primitive"
      forBlock := mth isMethodForBlock .
      homeMth := forBlock ifTrue:[ mth homeMethod ] ifFalse:[ mth ].
      envId := 1 "assumed env in which prim is installed".
      rubyPrimSel := RubyContext defaultOrNil ifNotNil:[:ctx|
                       ctx selectorForPrimMethod: homeMth 
                             envId: envId  rcvrClass: rcvrClass]  .
      rubyPrimSel ifNil:[  "use normal smalltalk rendering"
        str := mth _classAndSelectorNameWidth: nameWidth .
        (mth inClass ~~ (rcvCls := rcvrClass) and:[ rcvCls ~~ Object]) ifTrue:[
          str add: ' [' ; add: rcvCls name ; add: ']' .
        ].
      ] ifNotNil:[  | inCls |
        "rendering for smalltalk meth installed as a ruby primitive"
        forBlock ifTrue:[
          str := 'block in ', (homeMth inClass _rubyInspect: envId ) . 
        ] ifFalse:[ 
          str :=  (inCls := mth inClass) _rubyInspect: envId .
        ].
        str width: nameWidth ;
           addAll: ' # ' ; addAll: rubyPrimSel ; addAll: ' (ruby prim)'.
        inCls ifNotNil:[ (inCls ~~ (rcvCls := rcvrClass) and:[ rcvCls ~~ Object])  ifTrue:[
          str add: ' [' ; add: (rcvCls  _rubyInspect: envId) ; add: ']' .
        ]]. 
      ] 
    ] ifFalse:[
      str := mth _classNameAndRubySelectorWidth: nameWidth .
    ]
  ].
  ^ str

