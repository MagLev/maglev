*maglev-runtime
lookupRubyCallStarB: aSymbol env: envId
  | sym arr |
  sym := aSymbol prefixIfRubySelector _asSymbolWithRubySuffix: 16r3 "(s, '#0*&') asSymbol" .
  arr := self lookupSelector: sym rubyEnv: envId .
  ^ arr ifNotNil:[ arr at: 1 "ignore method protection details" ]

