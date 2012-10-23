*maglev-runtime
rubyCount: templates
  "A ruby primitive.
   Return a count of characters in the receiver as specified by templates."
  |characters count| 
  characters := BitSet forTemplates: templates env: 1"__callerEnvId" .
  count := 0.
  1 to: self size do:[ :i |
    ( characters at: (self codePointAt: i)) == 1  ifTrue:[ count := count +  1 ]
  ].
  ^ count.

