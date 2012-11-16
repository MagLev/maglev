*maglev-runtime
loadBootFileNamed: aString env: envId
  "load a file persistently, in bootstrap mode, 
   which disallows dynamic instVars 
   and does constant evaluation at compile time, if possible "
  | cst savePm savePath |
  cst := RubyCompilerState initialize: envId .
  savePm := cst persistenceMode .
  ^ [ cst installingPrims: true ; persistenceMode: true .
      self _addLibs: (RubyEnv _getenv:'MAGLEV_HOME') , '/src' env: envId .
      self loadFileNamed: aString env: envId
    ] ensure:[
      cst installingPrims: false ; persistenceMode: savePm .
      RubyFile loadPath: envId put: self defaultLoadPath .
    ]

