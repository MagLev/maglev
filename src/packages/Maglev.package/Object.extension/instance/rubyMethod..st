*maglev-runtime
rubyMethod: aSymbol
  "a ruby primitive, and called from generated code.
   Returns an instance of RubyMeth , or signals a NameError"
   |  m  |
   m := self virtualClass rubyMethodFor: aSymbol env: 1"__callerEnvId" .
   m object: self .
   ^ m

