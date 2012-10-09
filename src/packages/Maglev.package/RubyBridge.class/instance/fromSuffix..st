*maglev-runtime
fromSuffix:  aSymbol
      "ruby_selector_suffix dependent"

  fromNargs := (aSymbol codePointAt: 2) - 48"$0" .
  fromRestArg :=  (aSymbol at: 3) == $* .
  fromBlockArg := (aSymbol at: 4) == $& .

