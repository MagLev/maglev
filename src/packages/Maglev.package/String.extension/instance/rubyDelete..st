*maglev-runtime
rubyDelete: templates
  "a ruby primitive.
   Return a copy of the receiver, minus those characters specified by templates"
  | characters result resIdx  |
  characters := BitSet forTemplates: templates "bits == 1 are chars to delete"
                        env: 1"__callerEnvId" .
  result := self species new .
  resIdx := 1 .
  1 to: self size do:[ :i | | ch |
    ch := self codePointAt: i .
    (characters at:  ch  ) == 0 ifTrue:[ 
      result codePointAt: resIdx put: ch . 
      resIdx := resIdx + 1 
    ]
  ].
  ^ result.

