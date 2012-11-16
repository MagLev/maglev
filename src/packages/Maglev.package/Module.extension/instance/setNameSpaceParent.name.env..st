*maglev-runtime
setNameSpaceParent: aTransientNs name: aSymbol env: envId
  | tns |
  self changeNameTo: aSymbol .
  tns := self transientNameSpaceForStore: envId .
  tns parent: aTransientNs name: aSymbol.

