*maglev-runtime
installBridgesFor: rubySel in: aClass argsDescr: argsDescrInt 
    optArgs: optArgsDescr protection: protInt primKind: primKind env: envId
 "primKnd argument is one of  0:normal 1:primitive 2:primitive_nobridge 
                       4:boot 5:primitive during boot , 6: primitive_nobridge during boot .
  Returns selectorPrefix Symbol of rubySel .
 "
 | prefix destSuffix |
 prefix :=  rubySel rubySelectorPrefixSymbol . 
 destSuffix := (rubySel copyFrom:( prefix size + 1) to: rubySel size ) asSymbol .
 ^ self installBridgesForPrefix: prefix suffix: destSuffix selector: rubySel
    in: aClass argsDescr: argsDescrInt
    optArgs: optArgsDescr protection: protInt primKind: primKind env: envId

