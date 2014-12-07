*maglev-runtime
_rubySaveForReextend: mod
  "a ruby primitive"
  | envId storeKey tns pns parTns parPns store |
  envId := 1"__callerEnvId" .
  storeKey := #__ruby_modules_to_reextend .
  self _add: mod toStore: storeKey.
