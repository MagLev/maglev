*maglev-runtime
reimplementationAllowed: aSymbol for: aClass cst: aRubyCompilerState
      "ruby_selector_suffix dependent"
  | firstCh  |
  (RestrictedClasses includes: aClass) ifTrue:[
     aClass == SmallInteger ifTrue:[ |  sym  |
       sym := aSymbol prefixIfRubySelector  asSymbol .
       ( SmallIntSpecialSelectors includes: sym ) ifTrue:[ 
           ^ false "cannot reimplement  certain selectors like Fixnum#+   "
        ].
     ] ifFalse:[ "ExecBlock, GsNMethod or VariableContext, no extensions outside bootstrap"
       aRubyCompilerState installingPrims ifFalse:[ ^ false ].
     ].
  ].
  aSymbol size >= 7 ifTrue:[
    (firstCh := aSymbol at: 1) == $b ifTrue:[
       (aSymbol at:1 equals:'block_given?')  ifTrue:[
          aRubyCompilerState installingPrims ifFalse:[ ^ false ].
       ].
    ].
    (firstCh == $_ and:[ (aSymbol at: 2) == $_ ]) ifTrue:[
      ( ((aSymbol at: 1 equals: '__send__') and:[ aRubyCompilerState installingPrims not ] ) 
        or:[ (aSymbol last == $& and:[  aSymbol at: 1 equals: '__each#' ])
            "__each#0_& is prim handler for RubyBreakException installed in Object.rb"
        or:[ aSymbol at: 1 equals: '__perform#' ] ]
      ) ifTrue:[
         ^ false
       ].
    ].
  ].
  ^ true

