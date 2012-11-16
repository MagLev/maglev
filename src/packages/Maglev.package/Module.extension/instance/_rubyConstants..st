*maglev-runtime
_rubyConstants: envId
    "Return an IdentitySet of the names of the constants defined in this module "
   | ns |
   (ns := self nameSpace: envId) ifNotNil:[ ^ ns _rubyConstants].
   ^ IdentitySet new 

