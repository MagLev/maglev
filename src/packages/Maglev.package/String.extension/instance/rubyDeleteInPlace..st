*maglev-runtime
rubyDeleteInPlace: templates
  "A ruby primitive.
   Delete characters specified by templates from receiver."
  | characters destIdx recSize |
  characters := BitSet forTemplates: templates "bits == 1 are chars to delete"
                        env: 1"__callerEnvId" .
  destIdx := 0 .
  1 to: (recSize := self size) do:[ :n | 
    (characters at:  (self codePointAt: n )) == 1 ifTrue:[ " found first deletion point"
      destIdx := n - 1 .
       n + 1 to: recSize do:[ :k | | ch |
          ch := self codePointAt: k . 
          (characters at: ch ) == 0 ifTrue:[
              destIdx := destIdx + 1 .
              self codePointAt: destIdx put: ch 
          ].
       ].
       self size: destIdx . 
       ^ self 
    ]
  ].
  ^ nil  "no deletions found"

