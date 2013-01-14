*maglev-runtime
rubyInstvarNames

"A ruby primitive.
 Returns an Array of Strings which are names
 of named and dynamic instVars of the receiver visible to Ruby.
 names in the result start with '@' ."

| arr sz res  |
arr :=  self _instvarNamesAfter: self rubyPrivateSize .
res := Array new: (sz := arr size) .
1 to: sz do:[ :n | | sym |
   sym := arr at: n .
   (sym at: 1) == $@
     ifTrue:[  res at: n put:( String withAll: sym)]
     ifFalse:[ | nam symSiz |
       nam := String new: (symSiz := sym size) + 5 .
       nam replaceFrom: 1 to: 5 with: '@_st_' startingAt: 1 .
       nam replaceFrom: 6 to: symSiz + 5 with: sym startingAt: 1 .
       res at: n put: nam
   ].
].
^ res

