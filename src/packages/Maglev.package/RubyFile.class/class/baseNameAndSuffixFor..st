as yet unclassified
baseNameAndSuffixFor: aName
  "If aName ends with a valid ruby load file suffix (.rb, .so, .bundle, .dylib),
   return a pair of the name without the suffix and the suffix.
   E.g.  baseNameAndSuffixFor: 'abd.rb'  returns
   { 'abc' . '.rb' } If there is no valid suffix, then { aName . nil } is returned."

  | dotIdx baseName suffix sz |
  sz := aName size.
  dotIdx := aName indexOfLastByte: 46 "." startingAt: sz.
  dotIdx = 0 ifTrue: [^ {aName. nil}].
  baseName := aName copyFrom: 1 to: dotIdx - 1.
  suffix := aName copyFrom: dotIdx to: sz.
  (#('.rb' '.so' '.bundle' '.dylib') includes: suffix)
    ifTrue: [^ {baseName. suffix}]
    ifFalse: [^ {aName. nil}].

