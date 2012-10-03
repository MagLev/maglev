as yet unclassified
instVarExists: stName with: aNode 
  "return true if there is already a GsComVarLeaf for the specified instVar,
   or if aName specifies an instVar with a fixed offset" 
   |  leaf |
   leaf := leaves at: stName otherwise: nil .
   ^ leaf ifNotNil:[ true]  ifNil:[  (self indexOfInstVar: stName with: aNode) >= 0 ]

