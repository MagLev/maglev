*maglev-runtime
_runBenchmarks: logPath  repeat: repeatCount timeout: timeOut 
   "logPath is path to directory where stdout and report file for each benchmark will be written,
     timeOut is in seconds "
| saveDir skips saveStdout |
self error:'OBSOLETE, needs envId rework'.
saveDir := RubyDirectory _getwd .
[ | hm bmNum lf confFile status  ctx |
  hm := RubyEnv _getenv:'MAGLEV_HOME' .
  status := RubyDirectory _chdir: hm .
  status == 0 ifFalse:[ nil error:' chdir failed' ].
  ctx := RubyContext load .
  ctx persistentDo:[
    RubyContext requireFileNamed: 'benchmark/utils/bench.rb' env: 1 ;
         commitTransaction.
  ].
  bmNum := 1 .
  lf := Character lf .
  confFile := GsFile openReadOnServer: '$MAGLEV_HOME/benchmark/benchmark.conf' .
  saveStdout := Object transientNameSpace at: #STDOUT .
  [ true ] whileTrue:[
    | aLine fullPath bmResult stdoutFileName reportFileName |
    aLine := self nextConfFileLine: confFile  .
    aLine ifNil:[      ^ bmNum "hit eof" ].
    aLine := aLine trimWhiteSpace .
    aLine last == lf ifTrue:[ aLine size: (aLine size - 1) ].
    fullPath := hm , '/benchmark/' , aLine .
    GsFile gciLogServer:'--- start ', bmNum asString , ' :' , aLine .
    ProcessorScheduler _allProcessesTerminate9 . "terminate leftover GsProcesses "
    ctx abortResetTransient .
    stdoutFileName := logPath , 'benchStdout', bmNum asString , '.out' .
    reportFileName := logPath , 'bmreport', bmNum asString,'.txt' .
    GsFile redirectRubyStdout: ( GsFile openWriteOnServer: stdoutFileName ).
    bmResult := RubyContext runRuby: fullPath
        with: repeatCount asString , ' ' , timeOut asString , ' '  , reportFileName .
    GsFile redirectRubyStdout: saveStdout .
    GsFile gciLogServer:'--- end ' , aLine .
    bmNum := bmNum + 1 .
  ]
] ensure:[
  RubyDirectory _chdir: saveDir .
  saveStdout ifNotNil:[ GsFile redirectRubyStdout: saveStdout ] .
].

