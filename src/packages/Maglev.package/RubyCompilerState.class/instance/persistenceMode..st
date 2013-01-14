(as yet unclassified)
persistenceMode: aBoolean
   "returns the previous state"
  | prev |
  prev := persistenceMode .
  persistenceMode := aBoolean .
  ^ prev 

