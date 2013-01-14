*maglev-runtime
setNameSpaceParent: aTransientNs env: envId 
  | tns |
  tns := self transientNameSpaceForStore: envId .
  tns parent: aTransientNs 

