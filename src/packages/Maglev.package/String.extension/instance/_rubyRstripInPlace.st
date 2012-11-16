*maglev-runtime
_rubyRstripInPlace
 "delete trailing whitespace or NUL from receiver."
| sz cp |
(sz := self size) == 0 ifTrue: [ ^ nil ].
((cp := self codePointAt: sz ) == 0 or:[ cp codePointIsRubyWhitespace ]) ifFalse:[
  ^ nil 
].
sz - 1 _downTo: 1 do:[ :j |
  (self codePointAt: j) codePointIsRubyWhitespace ifFalse:[ "for 1.9 add   or:[ cp == 0 ]"
     self removeFrom: j + 1 to: sz . 
     ^ self 
  ].
].
self size: 0 .
^ self

