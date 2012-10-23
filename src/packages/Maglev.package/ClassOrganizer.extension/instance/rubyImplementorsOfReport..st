*maglev-runtime
rubyImplementorsOfReport: aSelector
      "ruby_selector_suffix dependent"
  | sym allCls implems  clsNames result pad | 
  allCls := self _allRubyClasses .
  sym := aSelector asSymbol prefixIfRubySelector .
  implems := { } .
  allCls do:[:aCls | | found |
     (aCls selectorsForEnvironment: 1) do:[ :aSel |
       (found == nil and:[ aSel prefixIfRubySelector == sym]) ifTrue:[ 
          implems add: aCls . found := true
        ]
     ]
  ].
  clsNames := { }.
  pad := '                                            ' .
  implems do:[:cls | | aCls str clsOop | 
      clsOop := (aCls := cls) asOop . 
      str := (aCls rubyFullName: 1) asString .
      str size == 0 ifTrue:[ "get smalltalk name"
        aCls isMeta ifTrue:[ str := 'meta' copy . aCls := aCls theNonMetaClass ].
        str addAll: aCls name . 
      ].
      str add: (pad copyFrom: 1 to: (40 - str size)) ;
           add: clsOop asString .
      clsNames add: str .
  ].
  clsNames := SortedCollection withAll: clsNames .
  result := String new . 
  clsNames do:[ :aLine | result add: aLine ; lf ].
  ^ result

