*maglev-runtime
walkWithScope: aScope
  | loc |
  (loc := location) ifNil:[ 
      (loc := aScope locationForName: name) ifNil:[
          self error:'local var not found' .
      ].
      location := loc 
  ].
  useToProc := loc varInfo readCount: 1 .

