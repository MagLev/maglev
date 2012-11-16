as yet unclassified
walkWithScope: aScope
   isBlockArg 
      ifTrue:[ location := aScope locationForBlockArg: name ]
     ifFalse:[  location := aScope locationForName: name ].
   super walkWithScope: aScope

