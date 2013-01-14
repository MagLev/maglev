*maglev-runtime
findOnLoadPath: aName env: envId
   (self loadPath: envId) do: [:pathPrefix || p loadPath sfx |
      loadPath := (pathPrefix terminatedWith: $/ ) , aName .
      p := self cannonicalPathFor: loadPath .
      (self isFile: p) ifTrue:[  ^ { p . loadPath } ].
    ].
    ^ nil

