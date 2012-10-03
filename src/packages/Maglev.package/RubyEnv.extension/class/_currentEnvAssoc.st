*maglev-runtime
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

