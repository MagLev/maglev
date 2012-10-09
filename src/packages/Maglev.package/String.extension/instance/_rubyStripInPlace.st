*maglev-runtime
_rubyStripInPlace
| first limit sz cp newSiz |
((sz := self size) == 0) ifTrue: [
   ^ nil "no modification made"
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
  first ifNil: [ self size: 0 . ^ self ].
].

((cp := self codePointAt: sz) == 0 or:[ cp codePointIsRubyWhitespace]) ifTrue: [
  sz - 1 _downTo: 1 do:[ :k |
    (self codePointAt: k) codePointIsRubyWhitespace ifFalse:[ "for 1.9 add   or:[ cp == 0 ]"
       self replaceFrom: 1 to: (newSiz := 1 + k - first) with: self startingAt: first .
       self size: newSiz .
       ^ self
    ].
  ].
].
first ~~ 1 ifTrue:[  
  newSiz := 1 + sz - first .
  self replaceFrom: 1 to: newSiz with: self startingAt: first .
  self size: newSiz .
  ^ self
].
^ nil "no modification made"

