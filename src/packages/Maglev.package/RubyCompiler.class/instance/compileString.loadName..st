*maglev-runtime
compileString: rubySource loadName: aName 
    "A ruby primitive.
    Used by  lib/ruby/site_ruby/1.8/maglev/ruby_compiler.rb  .
    Compile and run ruby source in sourceString. aName is the name passed to require or load.
    The parser uses aName to fill out __FILE__"
 | envId |
 envId := 1"__callerEnvId" .
^ [ | ast res cmState compStack prevSelf cm trace tmps warn |
    cmState := RubyCompilerState initialize: envId .
    (prevSelf := SessionTemps current at: #RubyMainSelf otherwise: nil) ifNil:[
        RubyLoadError signal:'compileString: cannot be used for the Ruby main program'
     ].
    trace := SessionTemps current at: #MagRpDEBUG otherwise: 0 .
    warn := SessionTemps current at:#MAGLEV_parseWarn otherwise: false .
    ast := RubyParserM rpParseString: rubySource path: aName loadName: aName 
                yTrace: trace - 1 warnings: warn .
    (compStack := cmState compilerStack) push: self .
    [  cm := self compileEvalMethod: #__compileFile  inClass: Object rubyMethod: ast  env: envId .
       res := prevSelf  performMethod:  cm  .
    ] ensure:[
       compStack pop: self .
       prevSelf ifNil:[ SessionTemps current at: #RubyMainSelf put: nil ].
    ].
    res
  ] onException: AbstractException do:[:ex |  | msg |
     msg := [   'error , ' , ex asString,  ',
          during ' , aName .
    ] onException: AbstractException do:[:exb |
       exb return: 'error during , ' , aName
    ].
    GsFile gciLogClient: msg .
    ex pass
  ].

