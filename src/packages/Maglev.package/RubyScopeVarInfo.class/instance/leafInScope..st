*maglev-runtime
leafInScope: scope
  ^ leaf ifNil:[ | lf |
     lf := scope _newLeaf: key offsetInScope: offsetInScop kind: kind .
     leaf := lf .
     lf
  ]

