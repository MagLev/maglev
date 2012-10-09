accessing
labelRedo: anObj
  labelRedo ~~ nil ifTrue:[ self error:'redefinition of labelRedo' ].
   labelRedo  := anObj

