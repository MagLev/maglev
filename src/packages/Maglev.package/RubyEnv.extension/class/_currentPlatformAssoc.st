*maglev-runtime
_currentPlatformAssoc
  ^ self _currentAssociation:  #RubyPLATFORMCurrent 
                 with: [ AbstractException cpuOsKindString ]

