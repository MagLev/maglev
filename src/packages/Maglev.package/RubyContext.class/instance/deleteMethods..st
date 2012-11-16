*maglev-runtime
deleteMethods: envId
  "used when forcing reloading of Ruby bootstrap code"
  (modifiedStClasses atOrNil: envId ) ifNotNil:[ :set |
    set do:[:aCls |
      aCls ~~ Object ifTrue:[
        aCls deleteMethods: envId .
        aCls virtualClass deleteMethods: envId .
      ]
    ].
  ].
  (rubyPrimMethods at: envId) removeAll 

