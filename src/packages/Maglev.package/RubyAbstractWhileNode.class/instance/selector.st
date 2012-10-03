*maglev-runtime
selector
      "ruby_selector_suffix dependent"
  | base sel |
  base := baseSel .
  blksInline ifTrue:[  " a ruby send"
     ^ base _asSymbolWithRubySuffix: 16r4 "#1__" 
  ].
  "a smalltalk send such as  #'_ruby_whileTrue: "
  sel := '_ruby_' , base .
  sel add: $: .
  ^ sel asSymbol  

