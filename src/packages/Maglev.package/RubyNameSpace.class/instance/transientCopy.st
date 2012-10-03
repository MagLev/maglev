*maglev-runtime
transientCopy
  | cls |
  ^ (cls := myClass) ifNil:[ self error:'transient copy not available' ]
                ifNotNil:[ cls transientNameSpaceForStore: envId ]

