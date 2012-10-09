*maglev-runtime
compileFileNamed: fullPath loadName: aName env: envId
        "The fullPath is the file to compile.  aName is the name passed to require or load.
         The parser uses aName to fill out __FILE__"
  | prevSelf |
^ [ | ast  res cst compStack defStk defCls |
    cst := RubyCompilerState initialize: envId .
    prevSelf  := GsProcess initRubyMainThread: installingPrims env: envId .
    ast := self parseFileNamed: fullPath loadName: aName .
    prevSelf ifNil:[ ast setMainProgram ].
    (compStack := cst compilerStack) push: self .
    [ | cm cld |
      cm := self compileEvalMethod: #__compileFile inClass: Object 
            rubyMethod: ast  env: envId .
      prevSelf ifNil:[
         self class withRubyHandlers: envId main: true do: [
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
       compStack pop: self .
       prevSelf ifNil:[ SessionTemps current at: #RubyMainSelf put: nil ].
    ].
    res
  ] onException: AbstractException do:[:ex |
    prevSelf ifNil:[
      [ | lf msg |
        lf := Character lf .
        (SessionTemps current at:#Maglev_ruby_debugFlag otherwise: false) ifTrue:[
           msg := 'error , ' , ex asString,  ',
             during ' , fullPath .
         ] ifFalse:[ |tns level|
           "Only print stack trace if warning level set and > 1"
           msg := nil .
           tns := Object transientNameSpaceForStore: 1 .
           level := tns rubyGlobalVar: #'$-W' .
           level ifNil: [ level := 0 ] .
           (level > 1) ifTrue: [
              (msg := ex @ruby1:inspect ) add: lf .
             (ex @ruby1:backtrace ) do:[:line | msg add: line; add: lf ].
           ] .
        ].
        msg ifNotNil: [ GsFile gciLogClient: msg ].
      ] onException: AbstractException do:[:exb |
          exb return: 'error during , ' , fullPath
      ].
    ].
    ex outer
  ].

