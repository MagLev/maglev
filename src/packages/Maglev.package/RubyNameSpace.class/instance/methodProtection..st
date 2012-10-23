*maglev-runtime
methodProtection: anInt 
  "anInt :  0==public, 1==protected, 2==private"
  "(anInt == 2 or:[ methodProtection == 2]) ifTrue:[ self pause ]. "

  ^ methodProtection := anInt 
  "not reflected to persistent name space "

