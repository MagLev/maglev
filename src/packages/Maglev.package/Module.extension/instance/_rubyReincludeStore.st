*maglev-runtime
_rubyReincludeStore
  | envId storeKey pns |
  envId := 1 .
  storeKey := #__ruby_modules_to_reinclude .
  pns := self persistentNameSpace: envId .
  pns ifNil: [ ^ nil ] .
  ^ pns at: storeKey ifAbsent: [ nil ]
