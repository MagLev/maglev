
set class BitSet
category: '*maglev-runtime'
classmethod:
forTemplates: templates env: envId

"Return the intersection of the BitSets for templates.  templates is an
array of template strings.  The BitSet for each template is calculated
using BitSet>>forTemplateString:."

| result tSiz |
(tSiz := templates size) == 0 ifTrue:[ ArgumentError signal:'too few args'].
result := self new: 256 . 
result bitInvert.
1 to: tSiz do:[ :n | | aStr |
       aStr := templates at: n .
       aStr _isOneByteString ifFalse:[
          aStr := self _coerceToString: aStr env: envId
       ].
       result bitAnd: (self forTemplateString:  aStr ) 
].
^result

%


set class BitSet
category: '*maglev-runtime'
classmethod:
forTemplateString: template

"Treat the template String as a pattern specifying a set of ASCII
characters.  Return a BitSet that represents those characters (bits are
indexed by the ASCII values of the characters).  If first character of
template is $^, then negate the sense (all characters except those in
template have their bits set).  $- between two other characters  expands to
all characters between the two adjacent characters, e.g., a-d expands to
abcd.  Only handles ASCII values, no multibyte.  Returns the new
BitSet."

|result start negating index limit|

result := self new: 256.
template size = 0 ifTrue:[ ^ result ].

negating := (template size > 1 and: [(template at: 1) == $^ ]).
start := negating ifTrue:[ 2 ] ifFalse:[ 1 ].
limit := template size.
index := start.
[ index <= limit ]
     whileTrue:[
         "Are we at the start of a character sequence? Check against
         (limit - 1) to ensure the $- is not a trailing one."
         (index < (limit - 1) and: [(template at: index + 1) == $- ])
             ifTrue:[
                 "We are expanding a character sequence, e.g.,  a-x"
                 (template codePointAt: index) to: (template codePointAt: (index + 2))
                 do:[:i| result at: i put: 1 ].
                 index := index + 2 ]
             ifFalse:[ result at: (template codePointAt: index) put: 1 ].
         index := index + 1 ].
negating ifTrue:[ result bitInvert ].
^ result.

%


set class BitSet
category: '*maglev-runtime'
classmethod:
_coerceToString: anObj env: envId
  ^ [ anObj @ruby1:to_str ] onException: Exception do:[:ex |
      ArgumentTypeError signal: 'coercion with to_str failed'
    ].

%

