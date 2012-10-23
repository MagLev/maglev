*maglev-runtime
initializeChildOf: parentScop name: sym 
  | assoc ps |
  name := sym .                 
  self parent: (ps := parentScop transientCopy)  .   
  envId := parentScop envId .
  
  "myClass, persistentCopy   left as nil" 
  methodProtection := 0 . 
  moduleFrozen := false .
  ^ self  

