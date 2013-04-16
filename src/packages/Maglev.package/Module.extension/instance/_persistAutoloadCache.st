*maglev-runtime
_persistAutoloadCache
  | pns tns |
  tns := self transientNameSpaceForStore: 1 .
  tns autoloadCache ifNotNil: [
    pns := tns persistentCopy .
    tns autoloadCache do: [ :assoc | |copy|
      copy := assoc copy .
      copy _value: nil .
      copy _isLoaded: false .
      pns addAssociation: copy.
    ]
  ]    