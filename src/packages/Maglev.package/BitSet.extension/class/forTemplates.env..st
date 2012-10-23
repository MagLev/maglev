*maglev-runtime
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

