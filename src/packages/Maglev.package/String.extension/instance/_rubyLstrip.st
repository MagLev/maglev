*maglev-runtime
_rubyLstrip

"Returns a new String containing the same Characters as the receiver,
 but with leading whitespace separators removed."

| sz |
(sz := self size) == 0 ifTrue:[ ^ self copy ].
(self codePointAt: 1) codePointIsRubyWhitespace ifFalse: [ ^ self copy ].
2 to: sz do:[:j |
  (self codePointAt: j) codePointIsRubyWhitespace ifFalse:[ 
    ^ self copyFrom: j to: sz
  ].
].
^ self class new

