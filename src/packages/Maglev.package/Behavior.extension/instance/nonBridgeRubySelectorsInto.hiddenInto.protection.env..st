*maglev-runtime
nonBridgeRubySelectorsInto: aSet hiddenInto: hiddenSet protection: protInt  env: envId
  "adds non-bridge ruby selectors of receiver to the set aSet .
   protInt specifies filtering, per GsNMethod>>rubyMethodProtection
     or -1 to return both public and protected. "
      "ruby_selector_suffix dependent"
  | sels |
  sels := self selectorsForEnvironment: envId .
  1 to: sels size do:[:idx | | mth aSel prefix |
    aSel := sels at: idx .
    mth := self compiledMethodAt: aSel rubyEnv: envId .
    mth ifNotNil:[
     "by using prefix, we trim the annotation characters.  Need to be careful of names like
     '&' and '&&', which are proper ruby method names.  This should be
      safe, since we don't strip if the method name has only one character
      (protects '&') and if the name is something like '&&', then it is a
      binary operator, and will be '&&:', and we trim $& before $:, which
      protects the '&&'.  Use rubySelectorPrefix from the .mcz .
     "
       prefix := aSel prefixIfRubySelector .
       (hiddenSet includes: prefix ) ifFalse:[
         mth _isSmallInteger ifTrue:[  
           mth <= METH_lu_skipPersistentMd ifTrue:[ 
             hiddenSet add: prefix .
           ] ifFalse:[ | override |
             override := METH_prot_override_public - mth.
             (override == protInt or:[ protInt == -1 and:[ override <= 1]]) ifTrue:[
               aSet add: prefix
             ] ifFalse:[
               hiddenSet add: prefix .
             ]
           ]
         ] ifFalse:[
            mth isRubyBridgeMethod ifFalse:[ | mp |
             (mp := mth rubyMethodProtection) == protInt ifTrue:[
               aSet add: prefix .
             ] ifFalse:[ 
               (protInt == -1 and:[ mp <= 1]) ifTrue:[
                  aSet add: prefix "returning both protected and public methods"
                ] ifFalse:[
                  hiddenSet add: prefix .
                ].
             ].
           ].
         ].
       ].
    ].
  ].

