*maglev-runtime
_rubySaveForReinclude: mod
  "a ruby primitive"
  | envId storeKey tns pns parTns parPns store |
  envId := 1"__callerEnvId" .
  storeKey := #__ruby_modules_to_reinclude .
  tns := self transientNameSpaceForStore: envId .
  ( parTns := tns parent) == tns ifTrue:[  ^ self  "do nothing  when self==Object" ].

  tns persistentCopy ifNil:[
    pns := RubyNameSpace new initializeForModule: self env: envId .
    parPns := parTns persistentCopy.
    pns parent: parPns .
    tns persistentCopy: pns .
    self persistentNameSpace: envId put: pns .
  ].

  pns := self persistentNameSpace: envId .
  store := pns at: storeKey ifAbsent: [ |tmpStore|
    tmpStore := Array new .
    pns at: storeKey runtimePut: tmpStore.
    tmpStore
  ].
  store add: mod .
