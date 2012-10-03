*maglev-runtime
setGlobals: envId

    "Sets $0 and ARGV from receiver."
    | tns argv |
    tns := Object transientNameSpaceForStore: envId .
    scriptName ifNotNil:[ tns rubyGlobalVar: #'$0' put: scriptName ].
    argv := tns at:#ARGV .
    argv size > 0 ifTrue:[ self error:'ARGV is non empty in RubyArgs>>setGlobals:'].
    argv addAll: scriptArgs .
    ^ self 

