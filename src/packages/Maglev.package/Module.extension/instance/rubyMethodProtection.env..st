*maglev-runtime
rubyMethodProtection: anInt env: envId
  "anInt :  0==public, 1==protected, 2==private"
  (self  transientNameSpaceForStore: envId) methodProtection: anInt

