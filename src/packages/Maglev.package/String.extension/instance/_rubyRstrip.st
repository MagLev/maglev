*maglev-runtime
_rubyRstrip

"Returns a new String containing the same Characters as the receiver,
 but with trailing whitespace or NUL removed."

| sz cp |

(sz := self size) == 0 ifTrue: [ ^ self copy ].
((cp := self codePointAt: sz ) == 0 or:[ cp codePointIsRubyWhitespace ]) ifFalse:[
  ^ self copy 
].
sz - 1 _downTo: 1 do:[ :j |
  (self codePointAt: j) codePointIsRubyWhitespace ifFalse:[ "for 1.9 add   or:[ cp == 0 ]"
    ^ self copyFrom: 1 to: j 
  ].
].
^ self class new

