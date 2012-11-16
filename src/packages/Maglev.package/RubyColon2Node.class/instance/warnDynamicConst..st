*maglev-runtime
warnDynamicConst: aKey 
  RubyContext bootWarnings == true ifTrue:[
      GsFile gciLogServer:
           'warning dynamic constant ', aKey , ', ',  self sourcePositionAsShortString .
  ].
  ^ true 

