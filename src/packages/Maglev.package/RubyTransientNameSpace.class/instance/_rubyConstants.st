as yet unclassified
_rubyConstants
  "Return an IdentitySet of the names of the constants defined"
  | names pns   |
  names := super _rubyConstants: nil .
  (pns := persistCopy) ifNotNil:[  pns _rubyConstants: names ].
  ^ names 

