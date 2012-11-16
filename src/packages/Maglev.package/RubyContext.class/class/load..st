*maglev-runtime
load: optionsArray
  | ctx |
  ctx := self load: optionsArray env: 1 .
  ^ ctx

