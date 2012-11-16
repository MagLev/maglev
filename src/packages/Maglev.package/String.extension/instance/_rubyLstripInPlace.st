*maglev-runtime
_rubyLstripInPlace

"delete leading whitespace separators from receiver"
| sz |
(sz := self size) == 0 ifTrue:[ ^ nil ].
(self codePointAt: 1) codePointIsRubyWhitespace ifFalse: [ ^ nil ].
2 to: sz do:[:j |
  (self codePointAt: j) codePointIsRubyWhitespace ifFalse:[
     self removeFrom: 1 to: j - 1 .
     ^ self.
  ].
].
self size: 0 . 
^ nil

