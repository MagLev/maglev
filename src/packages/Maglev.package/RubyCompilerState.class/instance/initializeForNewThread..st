*maglev-runtime
initializeForNewThread: envArg
   | tmps moduStk |
   envId := envArg .
   installingPrims := false .
   reloadingPrims := false .
   tmps := SessionTemps current .
   persistenceMode :=  tmps at: #MAGLEV_persistentFlag otherwise: false .
   persistableInstances := true .
   rtModuleStack := (moduStk := RubyCompilerStack new) .
   moduStk push: Object  .
   "fileStack and others left as nil"

