*maglev-runtime
_rubyStrip
| first limit sz cp |
((sz := self size) == 0) ifTrue: [
   ^ self copy
].
limit := sz + 1.
first := 1 .
(self codePointAt: 1) codePointIsRubyWhitespace ifTrue: [ | j |
  first := nil .
  j := 2.
  [ j == limit ] whileFalse: [
      (self codePointAt: j) codePointIsRubyWhitespace ifTrue: [
         j := j + 1.
      ] ifFalse:[
         first := j.
         j := limit .
       ].
  ].
  first ifNil: [ ^ self class new ].
].

((cp := self codePointAt: sz ) == 0 or:[ cp codePointIsRubyWhitespace ]) ifTrue:[
  sz - 1 _downTo: 1 do:[ :k |
    (self codePointAt: k) codePointIsRubyWhitespace ifFalse:[ "for 1.9 add   or:[ cp == 0 ]"
      ^ self copyFrom: first to: k
    ].
  ].
].
first == 1 ifTrue:[  ^ self copy ].
^ self copyFrom: first to: sz .

