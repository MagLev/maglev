*maglev-runtime
rubyCapitalize

"Return a copy of self with all but the first lower case"
|result|
self size == 0 ifTrue:[ ^ self species new ].
result := self asLowercase.
result at: 1 put: ((self at: 1) asUppercase).
^ result.

