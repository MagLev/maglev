*maglev-runtime
runFileNamed: aString env: envId
  | tns |
  tns := Object transientNameSpaceForStore: envId .
  aString ifNotNil:[ tns rubyGlobalVar: #'$0' put: aString ].
  ^ self loadFileNamed: aString env: envId .

