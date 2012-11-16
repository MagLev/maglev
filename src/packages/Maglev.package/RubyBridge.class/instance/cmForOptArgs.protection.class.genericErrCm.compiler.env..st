*maglev-runtime
cmForOptArgs: optArgsDescr protection: protInt class: aClass genericErrCm: genericErrCm 
    compiler: aComp env: envId
   | node cm isErr   | 
   (node := GsComMethNode newRuby)
     environment: envId ;
     selector_forBridge: from env: envId ;
     setRubyBridge ; 
     source: 
'<a Ruby bridge method>
  ' .
  self buildMethodArgumentsOn: node .
  isErr := self buildBodyOn: node argsDescr: optArgsDescr .
  (isErr and:[ genericErrCm ~~ nil and:[ protInt==0]]) ifTrue:[  ^ genericErrCm ] .

  node class: aClass ; 
         addMethodProtection: protInt .
  cm := aComp compiledMethodForIR: node .
  "cnt := System _sharedCounter: 1 incrementBy: 1 .  " " debugging stat"
  " GsFile gciLogServer:'bridge ' , cnt asString , ':  '  ,  fromSuffix , ' --> ' ,  to 
               , (isErr ifTrue:[ '   err '] ifFalse:[ ' ' ])  ."
  ^ cm

