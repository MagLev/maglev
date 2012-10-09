*maglev-runtime
_rubyInspect
  | str  |
  str := '#<' copy .  
  str addAll: self class name ;
     addAll: ':0x' ; addAll: self asOop hex ; add: $  ;
     addAll: self rubyStatus asString  ;
     add: $> .
  ^ str

