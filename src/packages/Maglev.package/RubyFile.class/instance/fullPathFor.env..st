*maglev-runtime
fullPathFor: aPathOrName env: envId
"Search for aPathOrName on the current ruby search path.  If aPathOrName is absolute,
  then ignore ruby search path and just cannonicalize and test for existence.
  Return the fully cannonicalized path for the file if found, otherwise return nil."

  (aPathOrName beginsWith: '/') ifTrue: [|cannon|
    cannon := RubyFile cannonicalPathFor: aPathOrName .
   (GsFile existsOnServer: cannon) 
     ifTrue:[^ cannon]
     ifFalse:[^ nil]
  ].

  "aPathOrName is relative, so search for it on ruby search path"
  (RubyFile loadPath: envId) do: [:pathPrefix | | p |
    p := RubyFile cannonicalPathFor: (pathPrefix terminatedWith: $/ ) , aPathOrName  .
    (RubyFile isFile: p) ifTrue:[ ^ p]
  ].
  ^ nil . "File not found"

