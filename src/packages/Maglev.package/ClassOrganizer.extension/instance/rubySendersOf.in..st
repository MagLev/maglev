*maglev-runtime
rubySendersOf: aSelector in: classList
      "ruby_selector_suffix dependent"
  | senders searcher prefix bridges |
  prefix := aSelector asSymbol prefixIfRubySelector.
  bridges := RubyBridge suffixOptionMasks asIdentitySet collect: [:m |
    prefix _asSymbolWithRubySuffix: m].
  senders := IdentitySet new.
  searcher := [:methDict :set :syms |
    methDict valuesDo: [:m |
      ((m isKindOf: GsNMethod) and: [m isRubyBridgeMethod not]) ifTrue: [
        (m _selectorPool * syms) ifNotEmpty: [set _addIfAbsent: m]]]].

  classList do: [:cls |
    { cls transientMethodDictForEnv: 1 .
      cls persistentMethodDictForEnv: 1 } do: [:ea |
        ea ifNotNilDo: [:methDict |
	  searcher value: methDict value: senders value: bridges]]].
  ^ senders asArray

