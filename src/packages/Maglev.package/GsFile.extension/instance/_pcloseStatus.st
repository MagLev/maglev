*maglev-runtime
_pcloseStatus
  "returns receiver"
  self _pclosePrimStatus ifNotNil:[ :s | | arr |
    arr := s == 0 ifTrue:[  { 16r100 . 1 . true . nil . s } ]
                  ifFalse:[ { 0 .     0 .  true . nil . s } ].
    GsProcess _current _rubyThreadDataAt: 2 "GC_RubyGsProcessClientData_childProcStatus"
        put:  (RubyProcessStatus with: arr ) . 
    "str := arr @ruby1:inspect .
     GsFile gciLogServer: 'pcloseStatus: ' , str . "
  ]

