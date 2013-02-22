*maglev-runtime
rubyImplementorsOf: aSelector in: allCls
      "ruby_selector_suffix dependent"
  | sym implems | 
  sym := aSelector asSymbol prefixIfRubySelector .
  implems := { } .
  allCls do:[:aCls | | found |
     (aCls selectorsForEnvironment: 1) do:[ :aSel |
       (found == nil and:[ aSel prefixIfRubySelector == sym]) ifTrue:[ 
          implems add: aCls . found := true
        ]
     ]
  ].
  ^ implems

