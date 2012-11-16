*maglev-runtime
rubyInstvarNames: envId 
  "Return an Array of Strings, the elements are names of
   dynamic class instance variables"
| list blk ns |
list := IdentitySet new .
blk := [ :aKey :aValue | 
  ((aKey at:1) == $@ and:[ aKey size >= 2 and:[ (aKey at: 2) ~~ $@ ]]) ifTrue:[ list add: aKey  ].
].
(ns := self transientNameSpace: envId) ifNotNil:[  ns keysAndValuesDo: blk ].
(ns := self persistentNameSpace: envId) ifNotNil:[ ns keysAndValuesDo: blk ].
list := list asArray .
1 to: list size do:[:n |  list at: n put: (String withAll: (list at:n)) ].
^ list

