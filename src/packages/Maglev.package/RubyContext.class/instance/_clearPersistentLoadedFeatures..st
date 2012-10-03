*maglev-runtime
_clearPersistentLoadedFeatures: envId

  ^ (Object persistentNameSpace: envId) rubyGlobalVar: #'$"' put: { } 

