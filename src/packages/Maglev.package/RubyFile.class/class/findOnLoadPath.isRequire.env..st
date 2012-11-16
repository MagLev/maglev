*maglev-runtime
findOnLoadPath: aName isRequire: isRequire  env: envId
  "Returns an Array { aRubyFile_orNil . featureName } "
  | names bns suffix searchForLibrary baseName defName|
  searchForLibrary := false.
  names := Array new.
  bns := RubyFile baseNameAndSuffixFor: aName.
  suffix := bns at: 2.
  baseName := bns at: 1.
  defName := aName.
  isRequire
    ifTrue: [ suffix
                ifNil: [ defName := aName, '.rb'.
                         names add: defName.
                         searchForLibrary := true ]
                ifNotNil: [ names add: aName. ].
              suffix = '.so' ifTrue: [ searchForLibrary := true ]]
    ifFalse: [ names add: aName ].

  searchForLibrary ifTrue: [ |suffixes|
    suffix = '.so'
      ifTrue: [ suffixes := #('.bundle' '.dylib') ]
      ifFalse: [ suffixes := #('.so' '.bundle' '.dylib') ].
    suffixes do: [:sfx| names add: (baseName , sfx) ]].

  names do: [:ea| |x|
    (self findOnLoadPath: ea env: envId) ifNotNil:[ :arr | | p loadPath fil |
          p := arr at: 1 .  loadPath := arr at: 2 .
          fil := self withGivenPath: ea fullPath: p loadName: loadPath.
          (#('.so' '.bundle' '.dylib') anySatisfy: [:s|  ea endsWith: s ])
             ifTrue: [ fil setIsSharedLib ].
          ^ { fil . ea }
    ].
  ].
  ^ { nil . defName }


