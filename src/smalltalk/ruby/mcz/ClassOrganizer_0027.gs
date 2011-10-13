
set class ClassOrganizer
category: '*maglev-runtime'
method:
rubySendersOf: aSelector
      "ruby_selector_suffix dependent"
  ^ self rubySendersOf: aSelector in: self _allRubyClasses .

%


set class ClassOrganizer
category: '*maglev-runtime'
method:
rubySendersOf: aSelector in: classList
      "ruby_selector_suffix dependent"
  | senders searcher |
  senders := IdentitySet new.
  searcher := [:methDict :set :sym |
    methDict valuesDo: [:meth || srcOffs |
      (meth isKindOf: GsNMethod) ifTrue: [
        srcOffs := meth _sourceOffsetOfFirstSendOf: sym.
        srcOffs ifNotNil: [set _addIfAbsent: meth]]]].

  classList do: [:cls |
    { cls transientMethodDictForEnv: 1 .
      cls persistentMethodDictForEnv: 1 } do: [:ea |
        ea ifNotNilDo: [:methDict |
	  searcher value: methDict value: senders value: aSelector]]].
  ^ senders asArray

%


set class ClassOrganizer
category: '*maglev-runtime'
method:
rubyImplementorsOf: aSelector
      "ruby_selector_suffix dependent"
  ^ self rubyImplementorsOf: aSelector in: self _allRubyClasses .

%


set class ClassOrganizer
category: '*maglev-runtime'
method:
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

%


set class ClassOrganizer
category: '*maglev-runtime'
method:
rubyImplementorsOfReport: aSelector
      "ruby_selector_suffix dependent"
  | implems clsNames result pad | 
  implems := self rubyImplementorsOf: aSelector .
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

%


set class ClassOrganizer
category: '*maglev-runtime'
method:
_allNamedRubyClasses: aClass into: anIdentitySet
  | result |
  result := IdentitySet new .
  [
    { aClass transientNameSpace: 1 . 
        aClass persistentNameSpace: 1  } do:[:ns |
      ns ifNotNil:[  
        ns keysAndValuesDo:[ :k :v | | child |
          (v isKindOf: Behavior) ifTrue:[ 
              child := v ] ifFalse:[
          (v isKindOf: RubyNameSpace) ifTrue:[ 
             v myClass ifNotNil:[:cls | child := cls ]
          ]].
          (anIdentitySet _addIfAbsent: child) ifTrue:[ 
            child == aClass ifFalse:[
              self _allNamedRubyClasses: child into: anIdentitySet
            ]
          ].
        ]
      ].
    ] 
  ] on: NameError do:[:ex | 
    "handle uninitialized Ruby class variables"
    ex resume: nil 
  ].

%


set class ClassOrganizer
category: '*maglev-runtime'
method:
_allRubyClasses
  | namedClasses set metaClasses |
  namedClasses := IdentitySet new .
  self _allNamedRubyClasses: Object into: namedClasses .
  metaClasses := IdentitySet new .
  namedClasses do:[:cls | "add all meta classes"
    metaClasses _addIfAbsent: (cls virtualClass) .
  ].
  namedClasses addAll: metaClasses .
  set := namedClasses copy .
  namedClasses do:[:aClass |  | cls | "add all superclasses"
    cls := aClass .
    [ 
      cls := cls rubySuperclass: 1 .
      cls ifNotNil:[
        cls isRubyModuleInclude ifTrue:[ 
          set _addIfAbsent: cls primaryCopy
        ] ifFalse:[
          set _addIfAbsent: cls
        ]
      ].
      cls ~~ nil 
    ] whileTrue 
  ].
  ^ set 

%


set class ClassOrganizer
category: '*maglev-runtime'
method:
_allRubyClasses: aClass
  | set |  
  set := IdentitySet new .
  [
    { aClass transientNameSpace: 1 . 
        aClass persistentNameSpace: 1  } do:[:ns |
      ns ifNotNil:[  
        ns keysAndValuesDo:[ :k :v | | child |
          (v isKindOf: Behavior) ifTrue:[ 
              child := v ] ifFalse:[
          (v isKindOf: RubyNameSpace) ifTrue:[ 
             v myClass ifNotNil:[:cls | child := cls ]
          ]].
          (set _addIfAbsent: child) ifTrue:[ 
            child == aClass ifFalse:[
              set addAll: (self _allRubyClasses: child)
            ]
          ].
        ]
      ].
    ] 
  ] on: NameError do:[:ex | 
    "handle uninitialized Ruby class variables"
    ex resume: nil 
  ].
  ^ set

%

