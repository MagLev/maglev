*maglev-runtime
extendClass: aClass rubyMethod: aNode env: envId
 | cld defStk rtModuStk |
  cld := GsProcess _current clientData .
  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: aClass .
  cld at: 7 put: aClass " _rubyThreadDataAt: 7 put: " .
  rtModuStk := cld at: 5  "_rubyThreadDataAt: 5, rtModuleStack" .
  rtModuStk push: aClass .
  ^ [ | cm |
      cm := self compileMethod: #'__compileClass' inClass: aClass class rubyMethod: aNode
         env: envId isEval: false .
      aClass performMethod: cm .
    ] ensure:[
      aClass rubyMethodProtection: 0 env: envId . "reset default prot to public" 
      defStk pop: aClass .
      rtModuStk pop: aClass .
    ].

