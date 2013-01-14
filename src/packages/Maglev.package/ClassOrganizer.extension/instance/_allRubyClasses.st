*maglev-runtime
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

