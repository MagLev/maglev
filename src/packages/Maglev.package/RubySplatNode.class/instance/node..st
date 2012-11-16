accessing
node: aNode 
   | n | 
   n := aNode .
   isLhsSplat ifNotNil:[ aNode ifNotNil:[  n := aNode adjustForLhsSplat ]].
   node := n

