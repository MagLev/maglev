ruby-support
compileFile: file
"The fullPath is the file to compile.  aName is the name passed to require or load.
   The parser uses aName to fill out __FILE__

This method emulates RubyCompiler>>compileFileNamed:loadName:env:
There is (essentially) only one line that changed:

   - ast := self parseFileNamed: fullPath loadName: aName .
   + ast := self astFor: file .

I think RubyCompiler>>compileFileNamed:loadName:env: should be refactored
to take an AST (regardless of how it came to be. A file is only one way
to get an AST).  Then All of this cut-n-paste code could go away, and be
replaced with:
    ast := RubyParserM astFor: aFile .   ""Method to be written""
    ^ RubyCompiler compileAst: ast .     ""Method to be written"""

  | prevSelf compiler installingPrims |
  installingPrims := false .
^ [ | ast  res cst compStack defStk defCls |
    cst := RubyCompilerState initialize: envId .
    prevSelf  := GsProcess initRubyMainThread: installingPrims env: envId .

    "ast := self parseFileNamed: fullPath loadName: aName . "
    ast := self astFor: file .

    prevSelf ifNil:[ ast setMainProgram ].
    compiler := RubyCompiler new .
    (compStack := cst compilerStack) push: compiler .
    [ | cm cld |
      cm := compiler compileEvalMethod: #__compileFile inClass: Object
            rubyMethod: ast  env: envId .
      prevSelf ifNil:[
         compiler class withRubyHandlers: envId main: true do: [
           | topSelf |
           topSelf := SessionTemps current at: #RubyMainSelf .
           cld := GsProcess _current clientData .
           (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: (defCls := topSelf class) .
           cld at: 7 put: defCls " _rubyThreadDataAt: 7 put: " .
           res := topSelf performMethod: cm  .
        ]
      ] ifNotNil:[  "recursed to load another file"
        cld := GsProcess _current clientData .
        (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: (defCls := prevSelf class) .
        cld at: 7 put: defCls " _rubyThreadDataAt: 7 put: " .
        res := prevSelf  performMethod:  cm  .
      ].
    ] ensure:[
       defStk ifNotNil:[ defStk pop: defCls ].
       compStack pop: compiler .
       prevSelf ifNil:[ SessionTemps current at: #RubyMainCompiler put: nil ].
    ].
    res
  ] onException: AbstractException do:[:ex |
    prevSelf ifNil:[
      [ | lf msg |
        lf := Character lf .
        (SessionTemps current at:#Maglev_ruby_debugFlag otherwise: false) ifTrue:[
           msg := 'error , ' , ex asString,  ',
             during ' , (file fullPath) .
         ] ifFalse:[ |level|
           "Only print stack trace if warning level set and > 1"
           msg := nil .
                         level := tns rubyGlobalVar: #'$-W' .
                         level ifNil: [ level := 0 ] .
                         (level > 1) ifTrue: [
                                 msg := ex perform: #inspect env: envId .
              msg add: lf .
             (ex perform: #backtrace env: envId ) do:[:line | msg add: line; add: lf ].
           ] .
        ].
        msg ifNotNil: [ GsFile gciLogClient: msg ].
      ] onException: AbstractException do:[:exb |
          exb return: 'error during , ' , (file fullPath)
      ].
    ].
    ex outer
  ].

