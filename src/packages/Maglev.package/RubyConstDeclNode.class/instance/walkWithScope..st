*maglev-runtime
walkWithScope: aScope
  |  leftDyn constn |
  leftDyn := (constn := constNode) walkWithScopeForCdecl: aScope .  
  (leftDyn == 0 and:[ aScope inBootstrap]) ifTrue:[ | aPath pSize |
    aPath := constn pathArray .
    (pSize := aPath size) == 1 ifTrue:[
      isDynamic := false .
      globalAssoc := aScope nameSpace bootAddConstAssociation: (aPath at: 1)
												env: (RubyCompilerState current envId) . 
    ] ifFalse:[
       pSize== 0 ifTrue:[ self error:'ConstDeclNode has empty path'].
       isDynamic := true .
    ]
  ] ifFalse:[
    isDynamic := true 
  ].
  super walkWithScope: aScope . "for valueNode"

