*maglev-runtime
compileIn: aClass rubyMethod: aNode
  "called from generated code"
   | cls selPrefix envId |
   cls := aClass ifNil:[ Object "for irb"  ].  
   envId := 1"__callerEnvId" . 
   selPrefix := self compileSelector: nil inClass: cls 
                rubyMethod: aNode env: envId  .
   selPrefix ifNotNil:[
     "ruby_selector_suffix dependent , use perform to bypass protection"
     cls with: selPrefix perform: #'method_added#1__' env: 1 .
   ].
   ^ nil  "per ruby specs, a  'def'  returns nil"

