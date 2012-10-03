accessing
irReceiverNodeEach: fullSelArray
  
  fullSelArray at: 1 put: #__superEach:  .
  RubyCompilerState current topCompiler setNeedsSuperEach .
  ^ GsComVariableNode newSelf

