*maglev-runtime
astDefaultVal
  | val |
  (val := valueNode) class == RubyLocalAsgnNode ifTrue:[
    ^ val astDefaultVal  "recurse to find right-most value in ruby   a=b=c=5 "
  ].
  ^ val

