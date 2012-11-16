*maglev-runtime
buildStatementsOn: irNode
    | nl last rtn  |
    self irNodeListInto: irNode .
    irNode lastAsRubyReturn .

