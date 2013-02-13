*maglev-runtime
suffixIfRubySelector
      "ruby_selector_suffix dependent, and send sites"
  (self _rubyOrdAt: -4) == 35"$#" ifTrue:[ ^ self rubySelectorSuffix ].
  ^ nil

