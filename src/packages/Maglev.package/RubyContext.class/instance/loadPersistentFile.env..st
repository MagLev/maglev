*maglev-runtime
loadPersistentFile: aString env: envId
  "load a file persistently, but without bootstrap flags"
  | cst savePm savePath |
  cst := RubyCompilerState initialize: envId .
  savePm := cst persistenceMode .
  savePath := (RubyFile loadPath: envId) copy .
^ [ cst persistenceMode: true .
    self _addLibs: (RubyEnv _getenv:'MAGLEV_HOME') , '/src' env: envId . 
    self loadFileNamed: aString env: envId
  ] ensure:[
    cst persistenceMode: savePm .
    RubyFile loadPath: envId put: savePath .
  ]

