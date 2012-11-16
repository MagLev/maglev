as yet unclassified
_value: aVal
  "returns aVal"
          " key == #Builder  ifTrue:[ self pause ].  "
  value := aVal . 
  isDefined := true .  "causes Ruby   defined?  operator to  return true"
  ^ aVal

