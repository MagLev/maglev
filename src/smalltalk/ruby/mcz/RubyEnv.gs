
set class RubyEnv class
category: '*maglev-runtime'
method:
_current
  "Return the current instance of RubyEnv , the value of ruby ENV ,
   for use within smalltalk code.  not used from generated code."

  ^ self _currentEnvAssoc _value

%


set class RubyEnv class
category: '*maglev-runtime'
method:
_currentAssociation: aKey with: aBlock
   | tempsDict assoc   |
assoc := (tempsDict := SessionTemps current) associationAt: aKey otherwise: nil .
assoc ifNil:[  | val |
  val := aBlock value.
  assoc := RubySymbolAssociation newWithKey: aKey .
  assoc _value:  aBlock value .
  tempsDict addAssociation: assoc .
].
^ assoc

%


set class RubyEnv class
category: '*maglev-runtime'
method:
_currentEnvAssoc
  ^ self _currentAssociation: #RubyENVCurrent with:
    [ | list dict siz |
       list := RubyEnv _getAllEnvVars .
       dict := RubyEnv _basicNew:( siz := list size) .
         1 to: siz  by: 2 do:[:n |
           dict at: (list at: n) put: (list at: n + 1) immediateInvariant
         ].
       dict
    ]

%


set class RubyEnv class
category: '*maglev-runtime'
method:
_currentPlatformAssoc
  ^ self _currentAssociation:  #RubyPLATFORMCurrent 
                 with: [ AbstractException cpuOsKindString ]

%

