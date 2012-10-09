*maglev-runtime
redirectRubyStdout: aGsFile env: envId
  "returns previous value of Ruby $stdout "
  | prev tns nam |
  nam := #'$stdout' . 
  prev := (tns := Object transientNameSpaceForStore:envId ) rubyGlobalVar: nam .
  tns rubyGlobalVar:  nam  put: aGsFile .
  ^ prev

