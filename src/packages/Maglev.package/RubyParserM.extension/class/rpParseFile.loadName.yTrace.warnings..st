*maglev-compiling
rpParseFile: fullPath  loadName: aName yTrace: yTrace warnings: warn
  | sourceString f |
  sourceString := (f := GsFile openReadOnServer: fullPath) contents.
  f close .
  sourceString immediateInvariant .
  ^ self rpParseString: sourceString path: fullPath loadName: aName 
		yTrace: yTrace warnings: warn

