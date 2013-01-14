*maglev-runtime
newModule
  "a ruby primitive"
  | mod |
  mod := self _newModule .
  mod name: #'' .
  mod init_parentNs: nil tns: nil  env: 1"__callerEnvId" .
  ^ mod

