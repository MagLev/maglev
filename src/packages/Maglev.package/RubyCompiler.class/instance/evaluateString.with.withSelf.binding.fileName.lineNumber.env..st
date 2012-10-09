*maglev-runtime
evaluateString: aString with: vcGlobalsArr withSelf: theSelf binding: aBinding 
  fileName: aFileName lineNumber: aLineNumber env: envId

  |   cst fileStk compStk aFile res methSrc  callerSel  evalSel
    fname forModuleEval lexPath prevLexPath rtModuStk evalScope"trac829" |
  envId < 1 ifTrue:[ self error: 'evaluateString: invalid envId' ].
  evalSel := self class blockEvalSelector.
      "ruby_selector_suffix dependent"
  callerSel := #'__evalCaller#2__' . "must agree with implem in bootstrap/Object.rb"
     "we will compile aString as main program level  ruby code so we don't get
      parse errors about dynamic constant defn or class def inside method (ticket 276)"
  methSrc := self evalMethodSource: aString with: vcGlobalsArr binding: aBinding .
  cst := RubyCompilerState initialize: envId .
  aBinding ifNotNil: [
    lexPath := aBinding lexicalPath ifNil:[ RubyLexicalPath withAll: { Object } ].
    (forModuleEval := aBinding forModuleEval) ifTrue:[ lexPath add: theSelf ].
    (evalScope := RubyEvalScope _basicNew) binding: aBinding . "trac 829"
  ] ifNil: [
    forModuleEval := false.  "lexPath, evalScope left as nil"
  ] .

  fname := aFileName ifNil:[ '(eval)' ].
  aFile := RubyEvalFile new source: methSrc  .
  aFile fileName: fname .
  (fileStk := cst fileStack) push: aFile .
  (compStk := cst compilerStack) push: self .
  rtModuStk := cst rtModuleStack .
  prevLexPath := rtModuStk copy .
  lexPath ifNotNil:[ rtModuStk size: 0 ; addAll: lexPath  ].
  [  |  cm prevSelf rubyRootNode |
        rubyRootNode :=  RubyParserM rpParseString: methSrc scope: evalScope
             lineBias: ( aLineNumber - self evalLineBias "extra lines for vcGlobals...")
         evalScope: evalScope fileName: fname .
        rubyRootNode fileName: fname  source: methSrc .
         "irMethodNode will be sent to  ast  later resulting in a zero arg method"
        cm := self compileEvalMethod: evalSel inClass: theSelf class
		  rubyMethod: rubyRootNode env: envId .
        vcGlobalsArr at:3 put: theSelf  .   "append/overwrite rcvr of the eval"
        prevSelf := GsProcess initRubyMainThread: false env: envId .
        prevSelf ifNil:[
          self class withRubyHandlers: envId main: true do: [
            res:= theSelf with: vcGlobalsArr with: cm perform: callerSel env: envId .
          ]
        ] ifNotNil:[
          res:= theSelf with: vcGlobalsArr with: cm perform: callerSel env: envId
        ]
  ] ensure:[
    compStk pop: self .
    fileStk pop: aFile .
    rtModuStk size: 0 ; addAll: prevLexPath .
  ].
  ^ res

