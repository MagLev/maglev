*maglev-runtime
_runPassingSpecs: confFileName verbose: verboseBool debugErrors: debugBool options: optsArr env: envId
  "for use from a topaz -l process, to run all passing specs,    stopping to debug any error.
   Returns the number of specs run "
| count ctx lf confFile saveDir |
saveDir := RubyDirectory _getwd .
[
  ctx := RubyContext _loadMspec: optsArr env: envId  .
  (RubyEnv _getenv:'USER') = 'otisa' ifTrue:[ | tmpDir status |
     tmpDir := (RubyEnv _getenv: 'GEMSTONE') , '/data/tmp' .
     (GsFile isServerDirectory: tmpDir) == true ifFalse:[
       status := RubyDirectory _mkdir: tmpDir permissions: 8r740 .
       status == 0 ifFalse:[ self error:' cannot create' , tmpDir ].
     ].
          "ruby_selector_suffix dependent"
     (RubyEnv _current) with: 'RUBYSPECTMP' with: tmpDir perform:#'[]=#2__' env:1
  ].
  count := 1 .
  lf := Character lf .
  confFile := GsFile openReadOnServer: '$MAGLEV_HOME/../svn/tests/rubytst/', confFileName .
  [ true ] whileTrue:[
    | aLine fullPath result pcu |
    aLine := self nextConfFileLine: confFile .
    aLine == nil ifTrue:[
       ^ count "DONE, hit eof"
    ].
    pcu := System _tempObjSpacePercentUsed .
    pcu > 80 ifTrue:[
      GsFile gciLogServer:'   ', pcu asString, '% temporary object memory used'
    ].
    GsFile gciLogServer:'--- start ', count asString , ' :' , aLine .
    ProcessorScheduler _allProcessesTerminate9 . "terminate leftover GsProcesses "
    ctx abortResetTransient .
    (Object transientNameSpaceForStore: envId) at: #DEBUG_SPEC runtimePut: debugBool  ;
                 at: #DEBUG_SPEC_VERBOSE runtimePut: verboseBool . 
    [  result := ctx _runSpec: aLine env: envId .
       GsFile gciLogServer:' <done>' .
    ] onException: AbstractException do:[:ex |
      GsFile gciLogServer:' <failed with error, ' , ex asString , '>' .
      (debugBool or:[ count == 1]) ifTrue:[ nil pause ].
    ].
    count := count + 1 .
  ].
] ensure:[
  RubyDirectory _chdir: saveDir
].

