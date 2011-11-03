
set class Number
category: '*maglev-runtime'
method:
asString
"provided to aid in topaz display of method temps, etc.
 In the base Smalltalk image, Number has no implementation of asString,
  and each numeric subclass provides its own implementation."

^ [
    self @env1:to_s 
  ] onException: AbstractException do:[:ex | 
    ex return: super asString
  ]

%


set class Number
category: '*maglev-runtime'
method:
_inspect
  ^ self asString

%

