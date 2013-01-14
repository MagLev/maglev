as yet unclassified
walkWithScope: aScope
  
  aScope inBootstrap ifTrue:[
	  "walk left node and resolve name for use in compileTimeClass"
	  ^ super walkWithScope: aScope  
  ] ifFalse:[
     |  isDynamic |
     (isDynamic := self determineDynamic ~~ 0)  ifTrue:[  
	   leftNode walkWithScope: aScope . "left node is to a Class or Module"
     ].
     ^ isDynamic
  ].

