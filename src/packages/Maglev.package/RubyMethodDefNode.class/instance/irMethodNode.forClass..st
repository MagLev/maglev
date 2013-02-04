*maglev-runtime
irMethodNode: envId forClass: aClass 
      "ruby_selector_suffix dependent"

^ irMethNode ifNil:[  "Fix Trac770, only run AST to IR once on a given AST"
    self buildIrMethodNode: [ :node | | mSel prefix  |
      irMethNode := node .
      node fileName: fileName source: source ;
         lineNumber:  startLine ; endSourceOffset: endOffset .
      self ir: node  "get start source offset".
      prefix := nameNode .  "a Symbol, not a RubyArgumentNode"
      scope extraArgs: argsNode extraArgs .
      self useScope: scope during:[ | lastNormalArgIdx |
         lastNormalArgIdx := argsNode buildMethodArgumentsOn: node implicitBlk: hasBlockArgRef .
         (prefix _rubyOrdAt: -1) ==  61"$= asciiValue"  ifTrue:[ | secLast |
           secLast := prefix _rubyOrdAt: -2 .
           secLast ifNotNil:[ 
             secLast := Character withValue: secLast .
             (secLast == $]  or:[ secLast isAlphaNumeric]) ifTrue:[
               "Example:  def foo=(a,b); end   # default return value is b , per Trac 785"
               node rubyDefaultReturnArg: lastNormalArgIdx .
             ].
           ].
         ].
         mSel := (prefix , argsNode selectorSuffix) asSymbol .
         methSelector := mSel .
         hasBlockArgRef ifTrue: [
	     "add a temp to keep the implicit block around. If we have an explicit block argument,
            s/o might assign to it, and we loose the original block reference
            (github.com/MagLev/maglev/issues/69)"
	     self optionallyBuildImplicitBlockTemp].
         scope buildTempsOn: node .
         self buildOptArgsOn: node.
         (prefix == #initialize or:[ prefix == #initialize_copy]) ifTrue:[
            aClass isMeta ifFalse:[
               "initialize, initialize_copy  instance methods  private by default."
               node setRubyPrivate .
            ].
         ].  
         defTarget ifNotNil:[ :dt | | rtCst snd |  "this is an outer def"
           (rtCst := GsComSendNode new)
              rcvr: (GsComLiteralNode newObject: RubyCompilerState) ;
              stSelector: #current .
           (snd := GsComSendNode new)
              rcvr: rtCst ;
              stSelector: #outerDefLexPath: ;
              appendArgument: (GsComLiteralNode newObject: 
            RubyCompilerState current rtModuleLexPath) .
           node appendStatement: snd .
       (snd := GsComSendNode new)
         rcvr: (GsComLiteralNode newObject: dt) ;
         stSelector:  self defTargetAssignSelector  ;
         appendArgument: ( GsComLiteralNode newObject: aClass) . 
       node appendStatement: snd .
         ].
         self bodyNode buildStatementsOn: node .
      ].
      sendsBinding == true ifTrue:[ node forceAllArgsTmpsToVc ].
    ]
  ]

