*maglev-runtime
extend: anObject rubyMethod: aNode blk: blkArg rtModulePath: rtModules
  "called from generated code, used for RubySClassNode "
  | envId cld defStk rtModuStk prevModules cls | 
  envId := 1"__callerEnvId" . 
  cls := anObject _singletonClass: envId .
  cld := GsProcess _current clientData .
  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: cls .
  cld at: 7 put: cls " _rubyThreadDataAt: 7 put: " .
  rtModuStk := cld at: 5  "_rubyThreadDataAt: 5, rtModuleStack" .
  prevModules := rtModuStk copy .
  rtModuStk size: 0 ; addAll: rtModules; add: cls .
  ^ [  | cm mArray |
      cm := self compileMethod: #'__compileClass' inClass: cls class 
              rubyMethod: aNode env: envId isEval: false .
      cls with: blkArg performMethod: cm  "pass blkArg for Trac 808"
    ] ensure:[
      defStk pop: cls .
      rtModuStk size: 0 ; addAll: prevModules .
      cls rubyMethodProtection: 0 env: envId . "reset default to public"
    ]

