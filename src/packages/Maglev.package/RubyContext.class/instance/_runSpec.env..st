*maglev-runtime
_runSpec: pathArg env: envId
 
 "run an individual spec file from  topaz -l  .
  aPath is relative to  git/spec/rubyspec/1.8 in a Gemstone internal checkout.
  assumes RubyContext(c)>>_loadMspec already done"

  | saveDir aPath |
  envId == 1 ifFalse:[ ArgumentError signal:' invalid envId'].
  aPath := pathArg trimWhiteSpace .
  [ saveDir := RubyDirectory _getwd .
    [  | hm fullName specCls status args|
       hm := RubyEnv _getenv:'MAGLEV_HOME' .
       status := RubyDirectory _chdir: hm, '/spec/mspec/lib' .
       status == 0 ifFalse:[ self error:' chdir failed' ].
       fullName :=  hm , '/spec/rubyspec/' , aPath  . 
       specCls := Object rubyConstAt: #MSpec env: envId .  
        args := RubyArgs forScript: fullName withRubyArgs: { } withScriptArgs: { } .
        args processArgsInto: self env: envId .
      ^ specCls @ruby1:runonespec: fullName  .
   ] onException: AbstractException do:[:ex |
      self pause .
   ]
 ] ensure:[
    RubyDirectory _chdir: saveDir
 ].

