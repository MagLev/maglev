*maglev-runtime
waitpid: pidInt flags: flagsInt
  "Calls waitpid() .
   Updates Ruby $? and returns an Array  { pid . status },
   else returns a SmallInteger errno."
| parr pid rawStatus |
parr := System _waitpid: pidInt flags: flagsInt .
parr _isArray ifTrue:[
  pid := parr at: 1 .
  rawStatus := parr at: 2 .
  pid == pidInt ifTrue:[ | arr |
    arr := { rawStatus .  (rawStatus bitAnd: 16rFF)"childStatus" .
           (pid == pidInt)"completedBool" . nil "errMsg". 0 "errno" } .
    GsProcess _current _rubyThreadDataAt: 2 "GC_RubyGsProcessClientData_childProcStatus"
              put:  (RubyProcessStatus with: arr ) .
  ].
].
^ parr  

