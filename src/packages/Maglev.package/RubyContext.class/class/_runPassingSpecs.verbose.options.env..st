*maglev-runtime
_runPassingSpecs: confFileName verbose: verboseBool options: optsArr env: envId
  ^ self _runPassingSpecs: confFileName verbose: verboseBool debugErrors: true 
            options: optsArr env: envId

