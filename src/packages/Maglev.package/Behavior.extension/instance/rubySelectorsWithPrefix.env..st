*maglev-runtime
rubySelectorsWithPrefix: prefix env: envId
    "Return set of all selectors matching the given prefix. Prefix may be a string or symbol"
      "ruby_selector_suffix dependent"
    |prefixSymbol sels|
    prefixSymbol := prefix asSymbol .
    sels := IdentitySet new .
    (self selectorsForEnvironment: envId) do:[ :ea |
        (ea prefixIfRubySelector)  == prefixSymbol ifTrue:[ 
            sels add: ea asSymbol .
        ]
    ] .
    ^ sels .

