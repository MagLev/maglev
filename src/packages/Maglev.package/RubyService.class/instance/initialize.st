initialization
initialize
  envId := 1 .
  ctx := RubyContext load: #() env: envId .
  tns := Object transientNameSpaceForStore: envId .
  tns rubyGlobalVar: #'$0' put: 'Smalltalk Invocation' .

