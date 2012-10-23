*maglev-runtime
abortResetTransient
  "Aborts transaction.  Resets transient state for env 1"
  | saveTrVals envId |
  envId := 1 .
  saveTrVals := self transientAssocsForReinit: envId .
  Module clearVmTransientState: envId . "clears all classes loaded in VM "
  Object _clearLookupCaches: envId ."invalidate all method lookup and send-site caches"
  System beginTransaction .  

  SessionTemps current removeKey: #RubyMainSelf ifAbsent:[] .
  RubyCompilerState clearTransientState .
  self reInitTransient: saveTrVals env: envId .

