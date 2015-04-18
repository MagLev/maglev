*maglev-runtime
_runVmUnit: parserHost options: optsArray  env: envId file: confFileName
"Run the vmunit tests from a topaz -l , assumming RubyContext load already done.
skips the tests listed in  skips array   below ."

| saveDir ctx skips saveStdout |
skips := { } .
ctx := RubyContext load: optsArray env: envId .
saveDir := RubyDirectory _getwd .
GsFile gciLogServer:'--- pwd= ', RubyDirectory _getwd .
[
  | hm count lf confFile logF status verbos |
  verbos := ( SessionTemps current at: #MagRpDEBUG otherwise: 0) > 1 .
  hm := RubyEnv _getenv:'MAGLEV_HOME' .
  status := RubyDirectory _chdir: hm .
  status == 0 ifFalse:[ self error:' chdir failed' ].
  count := 1 .
  lf := Character lf .
  confFile := GsFile openReadOnServer: hm , '/src/test/', confFileName .
  logF := GsFile openWriteOnServer: saveDir , '/vmunit.log' .
  saveStdout := (Object transientNameSpace: envId) at: #STDOUT .
  [ true ] whileTrue:[
    | aLine fullPath bmResult |
    [ | skip |
      aLine := self nextConfFileLine: confFile .
      aLine == nil ifTrue:[
      logF close .
      ^ count "hit eof"
      ].
      skip := ( skips detect:[:ea| aLine at:1 equals: ea ] ifNone:[ nil ]) ~~ nil .
      skip ifTrue:[  GsFile gciLogServer:'--- SKIPPING ' , aLine ].
      skip. 
    ] whileTrue .
    aLine last == lf ifTrue:[ aLine size: (aLine size - 1) ].
    fullPath := 'src/test/' , aLine .
    ProcessorScheduler _allProcessesTerminate9 . "terminate leftover GsProcesses "
    ctx abortResetTransient .
  " parserHost ifNotNil:[ | port |
      port := RubyCompiler parser port .
      RubyCompiler parser: (RubyParseTreeClient host: parserHost port: port ) .
    ].
  "
    GsFile gciLogServer:'--- begin ' , aLine .
    verbos ifFalse:[ GsFile redirectRubyStdout: logF env: envId ].
    bmResult :=  ctx runFileNamed: fullPath env: envId .
    verbos ifFalse:[ GsFile redirectRubyStdout: saveStdout env: envId ].
    count := count + 1 .
  ].
] ensure:[
  saveStdout ifNotNil:[ GsFile redirectRubyStdout: saveStdout env: envId ].
  RubyDirectory _chdir: saveDir
].

