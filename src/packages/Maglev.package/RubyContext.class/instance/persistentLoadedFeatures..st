*maglev-runtime
persistentLoadedFeatures: envId

  ^ (Object persistentNameSpace: envId) rubyGlobalVar: #'$"' 

