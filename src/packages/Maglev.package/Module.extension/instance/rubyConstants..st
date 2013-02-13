*maglev-runtime
rubyConstants: withIncluded
    "a ruby primitive.
     Return an Array of Strings, the names of constants defined in this module
       and any included modules."
    | moduleNames arr topCls cls envId |
    moduleNames := self _rubyConstants: (envId := 1"__callerEnvId") .
    cls := self .
    topCls := Object .
	withIncluded ifTrue: [
	    [ cls ~~ topCls ] whileTrue:[ 
    	  cls := cls rubySuperclass: envId .
      	cls ~~ topCls ifTrue:[ moduleNames addAll: (cls _rubyConstants: envId)]]].
    (arr := { }) _addAllFromNsc: moduleNames .
    1 to: arr size do:[:m |
      arr at: m put: (arr at: m) asString . "convert Symbols to Strings"
    ].
    ^ arr 

