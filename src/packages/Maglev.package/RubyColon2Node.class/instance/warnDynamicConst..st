*maglev-runtime
warnDynamicConst: aKey 
  RubyContext bootWarnings == true ifTrue:[
      | pos |
      pos := self sourcePositionAsShortString .
      GsFile gciLogServer:
           'warning dynamic constant ', aKey asString, ', ',  pos .
  ].
  ^ true 

