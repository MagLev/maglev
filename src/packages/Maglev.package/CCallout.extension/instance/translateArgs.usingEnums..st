*maglev-runtime
translateArgs: argsArray usingEnums: enumsInfo 
 "handles Symbol to int64 translation using named Enums"

 1 to: enumsInfo size by: 2 do:[ :n| | ofs anEnum v |
   ofs := enumsInfo at: 1 .
   anEnum := enumsInfo at: 2. 
   v := anEnum @ruby1:__sym_to_value: (argsArray at: ofs) .
   v ifNil:[ ArgumentError signal:'symbol not valid for specified argument Enum' ].
   argsArray at: ofs put: v .
 ].

