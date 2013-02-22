*maglev-runtime
setGlobals: envId

    "Sets $0 and ARGV from receiver."
    | tns argv dollarZero|
    tns := Object transientNameSpaceForStore: envId .
    self scriptlets
      ifEmpty: [scriptName ifNotNil: [dollarZero := scriptName]]
      ifNotEmpty: [dollarZero := '-e'].
    tns rubyGlobalVar: #'$0' put: dollarZero.
    argv := tns at:#ARGV .
    argv size > 0 ifTrue:[ self error:'ARGV is non empty in RubyArgs>>setGlobals:'].
    argv addAll: scriptArgs .
    ^ self 

