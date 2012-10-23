*maglev-runtime
reset: envId
  "To be used only to force subsequent reload of ruby bootstrap code.
   Returns true if a previous committed context was cleared"
  | prev found | 
  found := false .
  (prev := Saved) ifNotNil:[ 
    prev rubyContextReset: envId . "clear global name dicts in classes"
    found := true .
  ].
  ^ found

