*maglev-runtime
setProtection: protInt classmethods: selectorsArray
  "a ruby primitive"
  | sz count |
  (sz := selectorsArray size) ~~ 0 ifTrue:[
    | cls |
    cls := self isMeta ifTrue:[ self ] ifFalse:[ self class ] .
    count := cls setProtection: protInt methods: selectorsArray 
          env: 1"__callerEnvId" longSels: nil .
    count == sz ifFalse:[ 
      sz == 1 ifTrue:[ NameError signal: (selectorsArray at: 1), ' not found' ].
      NameError signal: 'one or more selectors not found'.
    ].
  ].
  ^ self 

