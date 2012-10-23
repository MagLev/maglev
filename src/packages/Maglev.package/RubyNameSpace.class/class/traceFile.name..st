*maglev-runtime
traceFile: prefix name: aName
  
  TraceGlobals >=1 ifTrue:[ 
     GsFile gciLogServer: prefix , (RubyFile pathForTrace: aName)
  ]

