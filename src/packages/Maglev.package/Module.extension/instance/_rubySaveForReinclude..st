*maglev-runtime
_rubySaveForReinclude: mod
  "a ruby primitive"
  | envId storeKey tns pns parTns parPns store |
  envId := 1"__callerEnvId" .
  storeKey := #__ruby_modules_to_reinclude .
  self _add: mod toStore: storeKey.
