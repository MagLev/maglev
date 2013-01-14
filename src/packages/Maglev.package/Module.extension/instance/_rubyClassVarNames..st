*maglev-runtime
_rubyClassVarNames: envId
  "Return an Array of Strings, the elements are names of
   ruby class variables"
| arr ns aCls  |
arr := { } .
aCls := self .
[ aCls ~~ nil ] whileTrue:[
  ns := aCls _classForRubyClassVar nameSpace: envId .
  ns ifNotNil:[
    ns  keysAndValuesDo:[ :aKey :aValue | 
      ((aKey at:1) == $@ and:[ aKey size > 2 and:[ (aKey at: 2) == $@ ]]) ifTrue:[ 
         arr add: (String withAll: aKey) 
      ].
    ].
  ].
  aCls := aCls rubySuperclass: envId
].
^ arr

