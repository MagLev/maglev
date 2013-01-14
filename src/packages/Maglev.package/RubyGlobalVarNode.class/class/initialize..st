as yet unclassified
initialize: envId
  "executed during bootstrap"
  envId == 1 ifTrue:[ self _initialize ].
  self sessionInitialize

