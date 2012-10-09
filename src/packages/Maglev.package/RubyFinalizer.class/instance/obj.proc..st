as yet unclassified
obj: anObject proc: aProc 
  "debugging only:  GsFile gciLogServer: 'RubyFinalizer self  ' , self asOop asString , 
                         ' obj ' , anObject asOop asString . "
  obj := anObject .
  procs := { aProc }.
  self beEphemeron: true . 
  ^ self .

