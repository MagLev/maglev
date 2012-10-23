*maglev-runtime
enclosingScope: aScope
    enclosingScope := aScope .
    inBootstrap := aScope inBootstrap  .
    variableNames := { } . "inline clearVariableNames"

