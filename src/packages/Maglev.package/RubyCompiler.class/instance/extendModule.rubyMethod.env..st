*maglev-runtime
extendModule: aModule rubyMethod: aNode env: envId
  | cld defStk rtModuStk |
  cld := GsProcess _current clientData .
  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: aModule .
  cld at: 7 put: aModule " _rubyThreadDataAt: 7 put: " .
  rtModuStk := cld at: 5  "_rubyThreadDataAt: 5, rtModuleStack" .
  rtModuStk push: aModule .
  ^ [ | cm | 
     cm := [ currClassOrModule := aModule .
       self compileMethod: #'__compileClass' inClass: aModule class 
                rubyMethod: aNode env: envId isEval: false .
     ] ensure:[
        currClassOrModule := nil .
     ].
     [ 
       aModule performMethod: cm .
     ] ensure:[
       aModule rubyMethodProtection: 0 env: envId; "reset default prot to public"
            disableModuleMethodsAll: envId  .
     ]. 
  ] ensure:[
    rtModuStk pop: aModule .
    defStk pop: aModule .
  ]

