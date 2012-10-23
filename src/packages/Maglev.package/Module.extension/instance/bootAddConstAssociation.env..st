*maglev-runtime
bootAddConstAssociation: aName env: envId 
  "returns an Association"
  
  ^ (self transientNameSpaceForStore:  envId ) bootAddConstAssociation: aName env: envId

