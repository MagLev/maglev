*maglev-runtime
_evalPrePostSources
 "Returns an Array of pre, post, and vc name components of source string
   for use in eval with a binding"
|  usesTheBinding |
usesTheBinding := true . "always, for Trac 829"

^ { ' '. ' ' . '__vc' . usesTheBinding }

