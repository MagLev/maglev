*maglev-runtime
_freezeModule: envId
  (self transientNameSpaceForStore: envId) freezeModule .
  (self transientMethodDictForStoreEnv: envId) immediateInvariant .

