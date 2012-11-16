*maglev-runtime
rubySelectorSuffix
    "Return a String containing the suffix of self"
      "ruby_selector_suffix dependent"
  | res |
  res := self _rubyAt1: -4 length: 4 .
  res ifNotNil:[
    (res at: 1) == $# ifTrue:[ ^ res ].
  ].
  Error signal:'missing ruby selector suffix'

