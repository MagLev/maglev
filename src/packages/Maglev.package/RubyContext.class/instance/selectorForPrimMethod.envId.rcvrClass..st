as yet unclassified
selectorForPrimMethod: meth envId: envId rcvrClass: rcvrClass
  "return the ruby selector for the given smalltalk method,
     if that method is installed as a ruby primitive, otherwise return nil"
| dict list aSel siz count multClasses firstCls |
dict := rubyPrimMethods at: envId .
list := dict at: meth otherwise: nil .
list ifNil:[ ^ nil ].
(siz := list size) == 2 ifTrue:[ ^ list at: 2"the rubySelector"].
count := 0 .
firstCls := list at: 1 .
1 to: siz by: 2 do:[:n | |aCls |
  (rcvrClass _rubySubclassOf:(aCls := list at: n) env: envId) ifTrue:[
    count := count + 1 .
    aSel := list at: n + 1 .
  ].
  aCls == firstCls ifFalse:[ multClasses := true ].
].
count == 1 ifTrue:[ ^ aSel ].
multClasses ifNil:[ ^ list at: 2 "the first entry" ].
^ nil.  "cannot determine specific selector"

